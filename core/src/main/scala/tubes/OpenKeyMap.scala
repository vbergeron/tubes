package tubes

/** The baseline `KeyMap`: keys of any types, over `arity` columns, in an open-addressing table with linear probing.
  * Slots hold key ids; each id keeps its key's hash and its values, one boxed value per column. A row's key is compared
  * with a slot's only when their hashes match, so only then are its values boxed.
  */
private[tubes] final class OpenKeyMap(arity: Int) extends KeyMap:
    private var slots: Array[Int]          = Vectors.minusOnes(KeyMap.capacity(16))
    private var mask: Int                  = slots.length - 1
    private var hashes: Array[Int]         = new Array(16)
    private val values: Array[Array[AnyRef]] = Array.fill(arity)(new Array[AnyRef](16))
    var size: Int                          = 0

    def insert(keys: IndexedSeq[Array[?]], n: Int): Array[Int] =
        val hs  = hash(keys, n)
        val ids = new Array[Int](n)
        var i   = 0
        while i < n do
            val h    = hs(i)
            var slot = h & mask
            while slots(slot) >= 0 && !matches(slots(slot), h, keys, i) do slot = (slot + 1) & mask
            if slots(slot) >= 0 then ids(i) = slots(slot)
            else
                ids(i) = add(h, keys, i)
                slots(slot) = ids(i)
                if size * 2 > slots.length then grow()
            i += 1
        ids

    def find(keys: IndexedSeq[Array[?]], n: Int): Array[Int] =
        val hs  = hash(keys, n)
        val ids = new Array[Int](n)
        var i   = 0
        while i < n do
            val h    = hs(i)
            var slot = h & mask
            while slots(slot) >= 0 && !matches(slots(slot), h, keys, i) do slot = (slot + 1) & mask
            ids(i) = slots(slot)
            i += 1
        ids

    def keyColumns: IndexedSeq[Array[?]] = values.toIndexedSeq.map(v => Vectors.specialize(v.take(size)))

    /** The hash of each row's key: the `##` of its values, combined column by column, then mixed. */
    private def hash(keys: IndexedSeq[Array[?]], n: Int): Array[Int] =
        val hs = new Array[Int](n)
        for column <- keys do
            (column: Any) match
                case a: Array[Int] =>
                    var i = 0
                    while i < n do
                        hs(i) = 31 * hs(i) + a(i).##
                        i += 1
                case a: Array[Long] =>
                    var i = 0
                    while i < n do
                        hs(i) = 31 * hs(i) + a(i).##
                        i += 1
                case a: Array[Double] =>
                    var i = 0
                    while i < n do
                        hs(i) = 31 * hs(i) + a(i).##
                        i += 1
                case a =>
                    var i = 0
                    while i < n do
                        hs(i) = 31 * hs(i) + Vectors.get(column, i).##
                        i += 1
        var i = 0
        while i < n do
            hs(i) = KeyMap.mix(hs(i))
            i += 1
        hs

    /** Whether key `id`, of hash `h`, is the key of row `i`. */
    private def matches(id: Int, h: Int, keys: IndexedSeq[Array[?]], i: Int): Boolean =
        if hashes(id) != h then false
        else
            var c = 0
            while c < arity && values(c)(id) == Vectors.get(keys(c), i) do c += 1
            c == arity

    /** Adds the key of row `i`, of hash `h`, under the next id. */
    private def add(h: Int, keys: IndexedSeq[Array[?]], i: Int): Int =
        if size == hashes.length then
            hashes = java.util.Arrays.copyOf(hashes, size * 2)
            for c <- 0 until arity do values(c) = java.util.Arrays.copyOf(values(c), size * 2)
        hashes(size) = h
        for c <- 0 until arity do values(c)(size) = Vectors.get(keys(c), i).asInstanceOf[AnyRef]
        size += 1
        size - 1

    /** Doubles the table, placing the keys again from their hashes. */
    private def grow(): Unit =
        slots = Vectors.minusOnes(slots.length * 2)
        mask = slots.length - 1
        var id = 0
        while id < size do
            var slot = hashes(id) & mask
            while slots(slot) >= 0 do slot = (slot + 1) & mask
            slots(slot) = id
            id += 1
