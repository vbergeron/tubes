package tubes

class RowKernelSuite extends munit.FunSuite:
    case class Item(id: Int, kind: String)

    val items = List(Item(1, "a"), Item(2, "b"), Item(3, "a"))
    val plan  = from(items).filter($.kind === "a").select(($.id, $.kind))

    test("collects with a standard collection companion"):
        val rows: List[(id: Int, kind: String)] = plan.materialize(RowKernel(List))
        assertEquals(rows, List((id = 1, kind = "a"), (id = 3, kind = "a")))
        val vector: Vector[(id: Int, kind: String)] = plan.materialize(RowKernel(Vector))
        assertEquals(vector.map(_.id), Vector(1, 3))
        val set: Set[(id: Int, kind: String)] = plan.materialize(RowKernel(Set))
        assertEquals(set.map(_.id), Set(1, 3))

    test("collects into arrays"):
        val array: Array[(id: Int, kind: String)] = plan.materialize(RowKernel(Array))
        assertEquals(array.toList.map(_.id), List(1, 3))
        val iarray: IArray[(id: Int, kind: String)] = plan.materialize(RowKernel(IArray))
        assertEquals(iarray.length, 2)
        assertEquals(iarray(1).kind, "a")

    test("collects grouped and joined rows"):
        val counts = from(items).groupBy($.kind)(count.as("n")).materialize(RowKernel(IArray))
        assertEquals(counts.toList, List((kind = "a", n = 2L), (kind = "b", n = 1L)))
        val pairs = from(items).join(from(List(Label("a", "alpha"))))($left.kind === $right.code)
        assertEquals(pairs.materialize(RowKernel(Vector)).map(_.label), Vector("alpha", "alpha"))

    test("streams through an iterator, running the plan only as far as rows are read"):
        var reads    = 0
        val source   = LazyList.from(Iterator.from(1).take(100).map(i => { reads += 1; Item(i, if i % 2 == 0 then "a" else "b") }))
        val streamed = from(source).filter($.kind === "a").select(($.id, $.kind)).take(10).materialize(RowKernel(Iterator))
        assertEquals(reads, 0)
        assertEquals(streamed.next(), (id = 2, kind = "a"))
        assertEquals(reads, 2)
        assertEquals(streamed.map(_.id).toList, List(4, 6, 8, 10, 12, 14, 16, 18, 20))
        assertEquals(reads, 20)

    test("streams grouped and joined rows through an iterator"):
        val counts = from(items).groupBy($.kind)(count.as("n")).materialize(RowKernel(Iterator))
        assertEquals(counts.toList, List((kind = "a", n = 2L), (kind = "b", n = 1L)))
        val pairs = from(items).leftJoin(from(List(Label("a", "alpha"))))($left.kind === $right.code)
        assertEquals(pairs.materialize(RowKernel(Iterator)).map(_.label).toList, List(Some("alpha"), None, Some("alpha")))

    case class Label(code: String, label: String)
