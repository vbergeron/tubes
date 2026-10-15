package tubes

import scala.compiletime.testing.typeCheckErrors

class JoinSuite extends munit.FunSuite:
    case class Item(sku: Int, color: String, size: Int)
    case class Stock(store: String, skuRef: Int, colorRef: String, qty: Int)
    case class Store(storeName: String, city: String)

    val items  = List(Item(1, "red", 10), Item(1, "blue", 12), Item(2, "red", 8))
    val stocks = List(
        Stock("north", 1, "red", 3),
        Stock("south", 1, "red", 0),
        Stock("north", 1, "blue", 5),
        Stock("north", 3, "red", 7)
    )
    val stores = List(Store("north", "Lyon"), Store("south", "Nice"))

    test("joins on one key, keeping left order and every match"):
        val plan = from(items).join(from(stocks))($left.sku === $right.skuRef).select(($.size, $.store))
        assertEquals(
            plan.materialize(RowKernel()),
            for size <- List(10, 12); store <- List("north", "south", "north") yield (size = size, store = store)
        )

    test("joins on a composite key, written either way round, with a residual predicate"):
        val plan = from(items)
            .join(from(stocks))(($right.colorRef === $left.color) && ($left.sku === $right.skuRef) && ($right.qty > $left.size - 10))
            .select(($.size, $.store, $.qty))
        assertEquals(
            plan.materialize(RowKernel()),
            List((size = 10, store = "north", qty = 3), (size = 12, store = "north", qty = 5))
        )

    test("hashes on the equalities and leaves the rest to the residual"):
        val plan = from(items).join(from(stocks))(($right.colorRef === $left.color) && ($left.sku === $right.skuRef))
        val keys = plan.asInstanceOf[JoinSpec[?]].keys
        assertEquals(keys.leftHash(items.head), keys.rightHash(stocks.head))
        assert(keys.matches(items.head, stocks.head))
        assert(!keys.matches(items.head, stocks(2)))
        assertEquals(keys.residual, None)

    test("joins without equalities pair every row, then filter"):
        val plan = from(items).join(from(stores))($left.size > 9).select(($.sku, $.city))
        assertEquals(plan.materialize(RowKernel()).size, 4)

    test("materializes joined rows as flat tuples"):
        val plan = from(stores).join(from(stocks))($left.storeName === $right.store && $right.qty === 7)
        assertEquals(plan.materialize(RowKernel()), List(("north", "Lyon", "north", 3, "red", 7)))
        assertEquals(plan.schema.names.toList, List("storeName", "city", "store", "skuRef", "colorRef", "qty"))

    test("joins chain, and `$` sees the joined schema"):
        val plan = from(items)
            .join(from(stocks))($left.sku === $right.skuRef && $left.color === $right.colorRef)
            .join(from(stores))($left.store === $right.storeName)
            .filter($.qty > 0)
            .select(($.color, $.city, ($.qty * 2).as("double")))
        assertEquals(
            plan.materialize(RowKernel()),
            List((color = "red", city = "Lyon", double = 6), (color = "blue", city = "Lyon", double = 10))
        )

    test("rejects joins whose field names clash"):
        val errors = typeCheckErrors("from(items).join(from(items))($left.sku === $right.sku)")
        assert(errors.exists(_.message.contains("their field names overlap")), errors.map(_.message))
