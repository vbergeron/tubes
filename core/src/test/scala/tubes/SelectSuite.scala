package tubes

class SelectSuite extends munit.FunSuite:
    case class Item(sku: Int, size: Int)

    val items = List(Item(1, 10), Item(2, 8))

    test("selects a named tuple, naming each column after its field"):
        val plan = from(items).select((id = $.sku, bigger = $.size + 1))
        assertEquals(plan.materialize(RowKernel()), List((id = 1, bigger = 11), (id = 2, bigger = 9)))
        assertEquals(plan.schema.names.toList, List("id", "bigger"))
