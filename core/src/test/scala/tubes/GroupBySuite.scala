package tubes

class GroupBySuite extends munit.FunSuite:
    case class Sale(shop: String, item: String, qty: Int, price: Double, units: Long, amount: BigDecimal)

    val sales = List(
        Sale("north", "apple", 3, 1.5, 30L, BigDecimal("4.50")),
        Sale("south", "pear", 1, 2.0, 10L, BigDecimal("2.00")),
        Sale("north", "pear", 2, 2.5, 20L, BigDecimal("5.00")),
        Sale("north", "apple", 5, 1.0, 50L, BigDecimal("5.00")),
        Sale("south", "apple", 4, 1.25, 40L, BigDecimal("5.00"))
    )

    test("groups by one key, in order of first appearance"):
        val plan = from(sales).groupBy($.shop)((count.as("n"), $.qty.sum.as("qty")))
        assertEquals(plan.materialize(RowKernel(List)), List((shop = "north", n = 3L, qty = 10), (shop = "south", n = 2L, qty = 5)))

    test("groups by several keys"):
        val plan = from(sales).groupBy(($.shop, $.item))(count.as("n"))
        assertEquals(
            plan.materialize(RowKernel(List)).map(r => (r.shop, r.item, r.n)),
            List(("north", "apple", 2L), ("south", "pear", 1L), ("north", "pear", 1L), ("south", "apple", 1L))
        )

    test("sums, averages, minimums and maximums of primitives"):
        val plan = from(sales).groupBy($.item)(
            ($.qty.sum.as("qty"), $.units.sum.as("units"), $.price.avg.as("avgPrice"), $.qty.min.as("least"),
                $.price.max.as("dearest"), $.units.max.as("most"))
        )
        val apple = plan.materialize(RowKernel(List)).head
        assertEquals((apple.item, apple.qty, apple.units, apple.least, apple.dearest, apple.most), ("apple", 12, 120L, 3, 1.5, 50L))
        assertEqualsDouble(apple.avgPrice, 1.25, 1e-9)

    test("aggregates other numeric types through their instances"):
        val plan = from(sales).groupBy($.shop)(($.amount.sum.as("total"), $.amount.min.as("smallest"), $.item.max.as("last")))
        assertEquals(
            plan.materialize(RowKernel(List)).map(r => (r.shop, r.total, r.smallest, r.last)),
            List(("north", BigDecimal("14.50"), BigDecimal("4.50"), "pear"), ("south", BigDecimal("7.00"), BigDecimal("2.00"), "pear"))
        )

    test("keys and aggregations can be computed expressions"):
        val plan = from(sales)
            .filter($.qty > 1)
            .groupBy(($.qty > 3).as("bulk"))(($.qty * 2).sum.as("double"))
        assertEquals(plan.materialize(RowKernel(List)), List((bulk = false, double = 10), (bulk = true, double = 18)))

    test("grouped rows feed further steps"):
        val plan = from(sales)
            .groupBy($.shop)($.qty.sum.as("qty"))
            .filter($.qty > 6)
            .select(($.shop, ($.qty * 10).as("scaled")))
        assertEquals(plan.materialize(RowKernel(List)), List((shop = "north", scaled = 100)))

    test("aggregations combine with the column algebra"):
        val plan = from(sales).groupBy($.shop)(
            (($.qty.max - $.qty.min).as("range"), ($.units.sum / 10 + count).as("mixed"), ($.qty.sum > 6).as("big"),
                ($.qty.sum / 4).as("quarter"))
        )
        assertEquals(
            plan.materialize(RowKernel(List)),
            List(
                (shop = "north", range = 3, mixed = 13L, big = true, quarter = 2),
                (shop = "south", range = 3, mixed = 7L, big = false, quarter = 1)
            )
        )

    test("ratios of aggregations, with fractional division and toDouble"):
        val plan = from(sales).groupBy($.item)(
            (($.price.sum / count.toDouble).as("meanPrice"), ($.qty.sum.toDouble / $.units.sum.toDouble).as("ratio"),
                ($.amount.sum / $.amount.max).as("share"))
        )
        val List(apple, pear) = plan.materialize(RowKernel(List)): @unchecked
        assertEquals((apple.item, pear.item), ("apple", "pear"))
        assertEqualsDouble(apple.meanPrice, 3.75 / 3, 1e-9)
        assertEqualsDouble(pear.ratio, 0.1, 1e-9)
        assertEquals(pear.share, BigDecimal("7.00") / BigDecimal("5.00"))

    test("aggregations are computed once however often they are used"):
        val plan = from(sales).groupBy($.shop)(
            ($.qty.sum.as("qty"), ($.qty.sum * 2).as("double"), ($.qty.sum + $.qty.max).as("plusMax"), count.as("n"))
        )
        assertEquals(plan.asInstanceOf[GroupBySpec[?]].compiled.slots, 3)
        assertEquals(
            plan.materialize(RowKernel(List)),
            List((shop = "north", qty = 10, double = 20, plusMax = 15, n = 3L), (shop = "south", qty = 5, double = 10, plusMax = 9, n = 2L))
        )

    test("aggregated columns can be literals or aggregations of computed columns"):
        val plan = from(sales).groupBy($.shop)((Column.lit(1).as("one"), ($.qty.toDouble / 2.0).avg.as("half")))
        assertEquals(plan.materialize(RowKernel(List)), List((shop = "north", one = 1, half = 5.0 / 3), (shop = "south", one = 1, half = 1.25)))

    test("fields must be aggregated in aggregations"):
        assert(compileErrors("from(sales).groupBy($.shop)(($.qty + $.qty.sum).as(\"x\"))").contains("Column qty must be aggregated"))

    test("aggregations are only computed by groupBy"):
        assert(compileErrors("from(sales).filter($.qty.sum > 3)").contains("only make sense over the rows of a group"))
        assert(compileErrors("from(sales).groupBy($.shop)($.qty.sum.sum.as(\"x\"))").contains("only make sense over the rows of a group"))

    test("aggregations must be named"):
        assert(compileErrors("from(sales).groupBy($.shop)($.qty.sum)").contains("every aggregation must be named"))
