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
        assertEquals(plan.materialize(RowKernel()), List((shop = "north", n = 3L, qty = 10), (shop = "south", n = 2L, qty = 5)))

    test("groups by several keys"):
        val plan = from(sales).groupBy(($.shop, $.item))(count.as("n"))
        assertEquals(
            plan.materialize(RowKernel()).map(r => (r.shop, r.item, r.n)),
            List(("north", "apple", 2L), ("south", "pear", 1L), ("north", "pear", 1L), ("south", "apple", 1L))
        )

    test("sums, averages, minimums and maximums of primitives"):
        val plan = from(sales).groupBy($.item)(
            ($.qty.sum.as("qty"), $.units.sum.as("units"), $.price.avg.as("avgPrice"), $.qty.min.as("least"),
                $.price.max.as("dearest"), $.units.max.as("most"))
        )
        val apple = plan.materialize(RowKernel()).head
        assertEquals((apple.item, apple.qty, apple.units, apple.least, apple.dearest, apple.most), ("apple", 12, 120L, 3, 1.5, 50L))
        assertEqualsDouble(apple.avgPrice, 1.25, 1e-9)

    test("aggregates other numeric types through their instances"):
        val plan = from(sales).groupBy($.shop)(($.amount.sum.as("total"), $.amount.min.as("smallest"), $.item.max.as("last")))
        assertEquals(
            plan.materialize(RowKernel()).map(r => (r.shop, r.total, r.smallest, r.last)),
            List(("north", BigDecimal("14.50"), BigDecimal("4.50"), "pear"), ("south", BigDecimal("7.00"), BigDecimal("2.00"), "pear"))
        )

    test("keys and aggregations can be computed expressions"):
        val plan = from(sales)
            .filter($.qty > 1)
            .groupBy(($.qty > 3).as("bulk"))(($.qty * 2).sum.as("double"))
        assertEquals(plan.materialize(RowKernel()), List((bulk = false, double = 10), (bulk = true, double = 18)))

    test("grouped rows feed further steps"):
        val plan = from(sales)
            .groupBy($.shop)($.qty.sum.as("qty"))
            .filter($.qty > 6)
            .select(($.shop, ($.qty * 10).as("scaled")))
        assertEquals(plan.materialize(RowKernel()), List((shop = "north", scaled = 100)))

    test("aggregations must be named"):
        assert(compileErrors("from(sales).groupBy($.shop)($.qty.sum)").contains("every aggregation must be named"))
