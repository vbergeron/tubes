package tubes

class TakeSkipSuite extends munit.FunSuite:
    case class Item(id: Int, kind: String)

    val items = List(Item(1, "a"), Item(2, "b"), Item(3, "a"), Item(4, "c"), Item(5, "a"))

    test("take keeps the first rows"):
        assertEquals(from(items).take(2).materialize(RowKernel()).map(_.id), List(1, 2))

    test("skip drops the first rows"):
        assertEquals(from(items).skip(3).materialize(RowKernel()).map(_.id), List(4, 5))

    test("skip then take pages through rows"):
        val page = from(items).skip(1).take(2).select($.id)
        assertEquals(page.materialize(RowKernel()), List((id = 2), (id = 3)))

    test("take then skip applies in order"):
        assertEquals(from(items).take(4).skip(3).materialize(RowKernel()).map(_.id), List(4))

    test("counts that are zero, negative or past the end"):
        assertEquals(from(items).take(0).materialize(RowKernel()), Nil)
        assertEquals(from(items).take(-1).materialize(RowKernel()), Nil)
        assertEquals(from(items).skip(-1).materialize(RowKernel()).size, 5)
        assertEquals(from(items).take(10).materialize(RowKernel()).size, 5)
        assertEquals(from(items).skip(10).materialize(RowKernel()), Nil)

    test("take applies after filter, select and groupBy"):
        val firstAs = from(items).filter($.kind === "a").take(2).select($.id)
        assertEquals(firstAs.materialize(RowKernel()), List((id = 1), (id = 3)))
        val topKind = from(items).groupBy($.kind)(count.as("n")).take(1)
        assertEquals(topKind.materialize(RowKernel()), List((kind = "a", n = 3L)))

    test("take and skip apply to join outputs"):
        case class Kind(name: String, label: String)
        val kinds = List(Kind("a", "alpha"), Kind("b", "beta"))
        val plan  = from(items).join(from(kinds))($left.kind === $right.name).skip(1).take(2).select(($.id, $.label))
        assertEquals(plan.materialize(RowKernel()), List((id = 2, label = "beta"), (id = 3, label = "alpha")))

    test("take stops reading its input once it has its rows"):
        var reads = 0
        val endless = LazyList.from(1).map: i =>
            reads += 1
            Item(i, "a")
        assertEquals(from(endless).take(3).materialize(RowKernel()).map(_.id), List(1, 2, 3))
        assert(reads <= 4, s"read $reads rows")
