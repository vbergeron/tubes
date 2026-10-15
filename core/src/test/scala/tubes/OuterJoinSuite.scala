package tubes

import scala.compiletime.testing.typeCheckErrors

class OuterJoinSuite extends munit.FunSuite:
    case class Item(sku: Int, color: String)
    case class Stock(skuRef: Int, store: String, qty: Int)
    case class Store(storeName: String, city: Option[String])

    val items  = List(Item(1, "red"), Item(2, "blue"), Item(3, "green"))
    val stocks = List(Stock(1, "north", 3), Stock(4, "south", 2), Stock(1, "south", 0), Stock(5, "east", 1))
    val stores = List(Store("north", Some("Lyon")), Store("west", None))

    test("left joins keep unmatched left rows, with `None` right fields"):
        val plan = from(items).leftJoin(from(stocks))($left.sku === $right.skuRef).select(($.sku, $.store))
        assertEquals(
            plan.materialize(RowKernel()),
            List(
                (sku = 1, store = Some("north")),
                (sku = 1, store = Some("south")),
                (sku = 2, store = None),
                (sku = 3, store = None)
            )
        )

    test("right joins keep unmatched right rows last, with `None` left fields"):
        val plan = from(items).rightJoin(from(stocks))($left.sku === $right.skuRef).select(($.color, $.skuRef))
        assertEquals(
            plan.materialize(RowKernel()),
            List(
                (color = Some("red"), skuRef = 1),
                (color = Some("red"), skuRef = 1),
                (color = None, skuRef = 4),
                (color = None, skuRef = 5)
            )
        )

    test("full joins keep unmatched rows of both sides"):
        val plan = from(items).fullJoin(from(stocks))($left.sku === $right.skuRef).select(($.sku, $.skuRef))
        assertEquals(
            plan.materialize(RowKernel()),
            List(
                (sku = Some(1), skuRef = Some(1)),
                (sku = Some(1), skuRef = Some(1)),
                (sku = Some(2), skuRef = None),
                (sku = Some(3), skuRef = None),
                (sku = None, skuRef = Some(4)),
                (sku = None, skuRef = Some(5))
            )
        )

    test("the residual decides what matches, so rows failing it come out unmatched"):
        val plan = from(items)
            .fullJoin(from(stocks))($left.sku === $right.skuRef && $right.qty > 0)
            .select(($.sku, $.store))
        assertEquals(
            plan.materialize(RowKernel()),
            List(
                (sku = Some(1), store = Some("north")),
                (sku = Some(2), store = None),
                (sku = Some(3), store = None),
                (sku = None, store = Some("south")),
                (sku = None, store = Some("south")),
                (sku = None, store = Some("east"))
            )
        )

    test("joins without equalities keep the left rows the condition rejects for all right rows"):
        val plan = from(items).leftJoin(from(stores))($left.sku > 2).select(($.sku, $.storeName))
        assertEquals(
            plan.materialize(RowKernel()),
            List(
                (sku = 1, storeName = None),
                (sku = 2, storeName = None),
                (sku = 3, storeName = Some("north")),
                (sku = 3, storeName = Some("west"))
            )
        )

    test("optional fields of an outer join nest their options"):
        val plan = from(stocks).leftJoin(from(stores))($left.store === $right.storeName).select(($.qty, $.city))
        assertEquals(
            plan.materialize(RowKernel()).map(_.city),
            List(Some(Some("Lyon")), None, None, None)
        )

    test("outer joins materialize as flat tuples, and their optional columns take defaults"):
        val plan = from(items).leftJoin(from(stocks))($left.sku === $right.skuRef)
        assertEquals(plan.materialize(RowKernel()).last, (3, "green", None, None, None))
        val totals = plan.filter($.qty.default(0) === 0).select(($.sku, $.store.default("none").as("store")))
        assertEquals(
            totals.materialize(RowKernel()),
            List((sku = 1, store = "south"), (sku = 2, store = "none"), (sku = 3, store = "none"))
        )

    test("outer joins chain, and later conditions see the optional fields"):
        val plan = from(items)
            .leftJoin(from(stocks))($left.sku === $right.skuRef)
            .leftJoin(from(stores))($left.store.default("") === $right.storeName)
            .select(($.sku, $.city))
        assertEquals(
            plan.materialize(RowKernel()),
            List(
                (sku = 1, city = Some(Some("Lyon"))),
                (sku = 1, city = None),
                (sku = 2, city = None),
                (sku = 3, city = None)
            )
        )

    test("rejects outer joins whose field names clash"):
        val errors = typeCheckErrors("from(items).fullJoin(from(items))($left.sku === $right.sku)")
        assert(errors.exists(_.message.contains("their field names overlap")), errors.map(_.message))
