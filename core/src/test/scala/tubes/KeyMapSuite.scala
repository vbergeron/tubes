package tubes

class KeyMapSuite extends munit.FunSuite:
    /** Inserts `batches` one after the other, then checks ids, lookups and key columns against a reference numbering. */
    def numbers(map: KeyMap, batches: Seq[IndexedSeq[Array[?]]])(using munit.Location): Unit =
        val seen = scala.collection.mutable.LinkedHashMap.empty[List[Any], Int]
        for keys <- batches do
            val n    = keys.head.length
            val rows = (0 until n).map(i => keys.map(Vectors.get(_, i)).toList)
            val ids  = map.insert(keys, n)
            assertEquals(ids.toList, rows.map(r => seen.getOrElseUpdate(r, seen.size)).toList)
            assertEquals(map.find(keys, n).toList, ids.toList)
        assertEquals(map.size, seen.size)
        val columns = map.keyColumns
        assertEquals((0 until map.size).map(id => columns.map(Vectors.get(_, id)).toList).toList, seen.keys.toList)

    test("numbers Int keys by direct addressing, then moves them to open addressing as their range widens"):
        val map = KeyMap(IndexedSeq(Array(1)))
        numbers(map, Seq(IndexedSeq(Array(5, 3, 5, 9)), IndexedSeq(Array(3, -2, 7))))
        // A key far away moves every key to open addressing, under the same ids.
        numbers(map, Seq(IndexedSeq(Array(5, 3, 5, 9, 3, -2, 7, 1 << 30, 9, Int.MinValue))))
        assert(map.keyColumns.head.isInstanceOf[Array[Int]])
        assertEquals(map.find(IndexedSeq(Array(4, 1 << 29)), 2).toList, List(-1, -1))

    test("numbers Long keys, and many of them, growing the tables"):
        val rnd = scala.util.Random(7)
        val dense  = (0 until 20).map(_ => IndexedSeq(Array.fill(5000)(rnd.nextInt(30000).toLong)))
        numbers(KeyMap(dense.head), dense)
        val sparse = (0 until 20).map(_ => IndexedSeq(Array.fill(5000)(rnd.nextLong() % 50000 * 1000003L)))
        numbers(KeyMap(sparse.head), sparse)
        numbers(LongKeyMap(ints = false), sparse)

    test("numbers keys of other types and of several columns, comparing them with =="):
        val strings = IndexedSeq[Array[?]](Array[AnyRef]("a", "b", "a", null, "c", null))
        numbers(KeyMap(strings), Seq(strings))
        val pairs = IndexedSeq[Array[?]](Array(1, 2, 1, 2, 1), Array[AnyRef]("x", "x", "x", "y", "y"))
        numbers(KeyMap(pairs), Seq(pairs, pairs))
        // Boxed and primitive integers are the same keys, as `==` has them.
        val map = OpenKeyMap(1)
        map.insert(IndexedSeq(Array(1, 2, 3)), 3)
        assertEquals(map.find(IndexedSeq(Array[AnyRef](Integer.valueOf(2), "2")), 2).toList, List(1, -1))

    test("numbers many keys of several columns, growing the table"):
        val rnd     = scala.util.Random(11)
        val batches = (0 until 10).map: _ =>
            IndexedSeq[Array[?]](Array.fill(4000)(rnd.nextInt(300)), Array.fill[AnyRef](4000)(s"k${rnd.nextInt(50)}"))
        numbers(KeyMap(batches.head), batches)

    test("numbers the single key of no columns"):
        val map = KeyMap(IndexedSeq.empty)
        assertEquals(map.insert(IndexedSeq.empty, 3).toList, List(0, 0, 0))
        assertEquals(map.find(IndexedSeq.empty, 2).toList, List(0, 0))
        assertEquals(map.size, 1)
