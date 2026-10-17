package tubes

import NamedTuple.AnyNamedTuple

/** The batch kernel must give the rows the row kernel gives, in the same order, whatever the batch size. */
class BatchKernelSuite extends munit.FunSuite:
    case class Item(sku: Int, color: String, size: Int)
    case class Stock(store: String, skuRef: Int, colorRef: String, qty: Int)
    case class Store(storeName: String, city: Option[String])
    case class Sale(shop: String, item: String, qty: Int, price: Double, units: Long, amount: BigDecimal)
    case class Person(name: String, age: Option[Int], nickname: Option[String])

    val items  = List(Item(1, "red", 10), Item(1, "blue", 12), Item(2, "red", 8), Item(3, "green", 0))
    val stocks = List(
        Stock("north", 1, "red", 3),
        Stock("south", 1, "red", 0),
        Stock("north", 1, "blue", 5),
        Stock("north", 3, "red", 7),
        Stock("east", 5, "red", 1)
    )
    val stores = List(Store("north", Some("Lyon")), Store("west", None))
    val sales  = List(
        Sale("north", "apple", 3, 1.5, 30L, BigDecimal("4.50")),
        Sale("south", "pear", 1, 2.0, 10L, BigDecimal("2.00")),
        Sale("north", "pear", 2, 2.5, 20L, BigDecimal("5.00")),
        Sale("north", "apple", 5, 1.0, 50L, BigDecimal("5.00")),
        Sale("south", "apple", 4, 1.25, 40L, BigDecimal("5.00"))
    )
    val people = List(Person("alice", Some(30), Some("al")), Person("bob", None, None), Person("carol", Some(15), None))

    /** Checks the batch kernel against the row kernel, with batches of 1, 2 and the default size. */
    def same[Out <: AnyNamedTuple](plan: Plan[Out])(using munit.Location): Unit =
        val expected = plan.materialize(RowKernel(List))
        for size <- List(1, 2, BatchKernel.DefaultBatchSize) do
            assertEquals(plan.materialize(BatchKernel(List, size)), expected, s"batch size $size")

    test("filters and selects, with arithmetic, comparisons and literals"):
        same(from(items).filter(($.size > 9) || ($.color === "green")).select(($.sku, ($.size * 2 + 1).as("odd"))))
        same(from(sales).select(($.item, ($.price * 2.0 - 0.5).as("p"), ($.units / 3L).as("third"), $.qty.toDouble.as("d"))))
        same(from(sales).filter(!($.qty <= 2) && $.price >= 1.25).select(($.shop, ($.amount + BigDecimal(1)).as("more"))))

    test("the right side of && and || is only read where the left side does not decide"):
        same(from(items).filter(($.size =!= 0) && (Column.lit(100) / $.size > 9)))
        same(from(items).filter(($.size === 0) || (Column.lit(100) / $.size < 9)))

    test("optional fields and defaults"):
        same(from(people).select(($.name, $.age, $.age.default(0).as("a"), $.nickname.default($.name).as("called"))))
        same(from(people).filter($.age === Some(30)).select(Tuple1($.name)))
        same(from(people).filter($.age.default(18) >= 18).select(Tuple1(($.age.default(18) + 1).as("next"))))

    test("take and skip, across batches"):
        for n <- List(-1, 0, 1, 2, 3, 10) do
            same(from(items).take(n))
            same(from(items).skip(n))
        same(from(items).skip(1).take(2).select($.sku))

    test("inner joins: one key, composite keys either way round, residuals, no equalities"):
        same(from(items).join(from(stocks))($left.sku === $right.skuRef).select(($.size, $.store)))
        same(
            from(items)
                .join(from(stocks))(($right.colorRef === $left.color) && ($left.sku === $right.skuRef) && ($right.qty > $left.size - 10))
        )
        same(from(items).join(from(stocks))(($left.sku + 2) === ($right.skuRef + 1) && !($right.qty === 0)))
        same(from(items).join(from(stores))($left.size > 9).select(($.sku, $.storeName)))

    test("outer joins keep unmatched rows, with optional fields"):
        same(from(items).leftJoin(from(stocks))($left.sku === $right.skuRef))
        same(from(items).rightJoin(from(stocks))($left.sku === $right.skuRef))
        same(from(items).fullJoin(from(stocks))($left.sku === $right.skuRef && $right.qty > 0))
        same(from(items).leftJoin(from(stores))($left.sku > 2).select(($.sku, $.storeName)))
        same(from(stocks).leftJoin(from(stores))($left.store === $right.storeName).select(($.qty, $.city)))

    test("joins chain and feed further steps"):
        same(
            from(items)
                .leftJoin(from(stocks))($left.sku === $right.skuRef)
                .leftJoin(from(stores))($left.store.default("") === $right.storeName)
                .filter($.qty.default(0) > 0)
                .select(($.sku, $.city))
        )

    test("groups by one or several keys, in order of first appearance"):
        same(from(sales).groupBy($.shop)((count.as("n"), $.qty.sum.as("qty"))))
        same(from(sales).groupBy(($.shop, $.item))(count.as("n")))
        same(from(sales).filter($.qty > 1).groupBy(($.qty > 3).as("bulk"))(($.qty * 2).sum.as("double")))

    test("aggregations of primitives and of other types"):
        same(
            from(sales).groupBy($.item)(
                ($.qty.sum.as("qty"), $.units.sum.as("units"), $.price.sum.as("price"), $.price.avg.as("avgPrice"),
                    $.qty.min.as("least"), $.price.max.as("dearest"), $.units.max.as("most"))
            )
        )
        same(from(sales).groupBy($.shop)(($.amount.sum.as("total"), $.amount.min.as("smallest"), $.item.max.as("last"))))

    test("aggregations combine with the column algebra and feed further steps"):
        same(
            from(sales).groupBy($.shop)(
                (($.qty.max - $.qty.min).as("range"), ($.units.sum / 10 + count).as("mixed"), ($.qty.sum > 6).as("big"),
                    ($.qty.sum * 2).as("double"), Column.lit(1).as("one"), ($.qty.toDouble / 2.0).avg.as("half"))
            )
        )
        same(from(sales).groupBy($.shop)($.qty.sum.as("qty")).filter($.qty > 6).select(($.shop, ($.qty * 10).as("scaled"))))
        same(from(sales).groupBy($.item)(($.price.sum / count.toDouble).as("meanPrice")).take(1))

    test("streams batches, reading no further batch than take needs"):
        var reads   = 0
        val endless = LazyList.from(1).map: i =>
            reads += 1
            Item(i, "red", i)
        assertEquals(from(endless).take(3).materialize(BatchKernel(List, 2)).map(_.sku), List(1, 2, 3))
        assert(reads <= 4, s"read $reads rows")

    test("exposes the batches, column by column"):
        val batches = BatchKernel.batches(from(items).select(($.sku, ($.size * 2).as("double"))), 3).toList
        assertEquals(batches.map(_.size), List(3, 1))
        assert(batches.head("double").isInstanceOf[Array[Int]])
        assertEquals(batches.head("double").asInstanceOf[Array[Int]].toList, List(20, 24, 16))
