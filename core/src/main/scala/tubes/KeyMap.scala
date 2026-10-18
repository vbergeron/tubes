package tubes

/** Numbers the distinct keys read from key columns, a batch at a time: each new key gets the next id, from 0, so ids
  * follow the order keys first appear in. Group-bys use ids as group numbers, hash joins to find a key's right rows.
  *
  * Keys are compared with `==` and hashed with `##`, as the row kernel compares join keys.
  */
private[tubes] trait KeyMap:
    /** The number of distinct keys inserted so far; their ids are `0 until size`. */
    def size: Int

    /** The id of the key of each of the first `n` rows of the `keys` columns, inserting the keys not seen yet. */
    def insert(keys: IndexedSeq[Array[?]], n: Int): Array[Int]

    /** The id of the key of each of the first `n` rows of the `keys` columns, or `-1` where it was never inserted. */
    def find(keys: IndexedSeq[Array[?]], n: Int): Array[Int]

    /** The key columns, holding each distinct key at its id. */
    def keyColumns: IndexedSeq[Array[?]]

private[tubes] object KeyMap:
    /** A map for keys read from columns like `sample`. A single `Int` or `Long` column is numbered by direct addressing
      * while its keys span a narrow range, then by a `LongKeyMap`; other keys go to an `OpenKeyMap`.
      */
    def apply(sample: IndexedSeq[Array[?]]): KeyMap = sample match
        case IndexedSeq(_: Array[Int])  => Integers(ints = true)
        case IndexedSeq(_: Array[Long]) => Integers(ints = false)
        case _                          => OpenKeyMap(sample.size)

    /** A map for keys read from columns like `sample`, through the loops `codec` generates for their types where there
      * are some, as `apply(sample)` picks one otherwise.
      */
    def apply(sample: IndexedSeq[Array[?]], codec: Option[KeyCodec]): KeyMap = codec match
        case Some(c) => Compiled(KeyTable(c))
        case None    => apply(sample)

    /** Spreads the bits of `h` over an `Int`, so that close keys land far apart in a table (MurmurHash3's finalizer). */
    inline def mix(h: Long): Int = KeyTable.mix(h)

    /** The smallest power of two above `2 * n`: tables stay at most half full. */
    def capacity(n: Int): Int = KeyTable.capacity(n)

    /** Keys numbered by a table whose loops were generated for their types. */
    private final class Compiled(table: KeyTable) extends KeyMap:
        def size: Int                                            = table.size
        def insert(keys: IndexedSeq[Array[?]], n: Int): Array[Int] = table.insert(keys, n)
        def find(keys: IndexedSeq[Array[?]], n: Int): Array[Int]   = table.find(keys, n)
        def keyColumns: IndexedSeq[Array[?]]                     = table.keyColumns

    /** The widest range of keys numbered by direct addressing: 2^27 ids, 512 MB. */
    private val MaxDenseRange = 1L << 27

    /** Keys of one `Int` (`ints`) or `Long` column. They are numbered by a `DenseKeyMap` while the range they span stays
      * under `max(2^20, 4 × rows read)` and `MaxDenseRange`, and by a `LongKeyMap` once it grows past either. The keys
      * move over in id order, so they keep their ids.
      */
    private final class Integers(ints: Boolean) extends KeyMap:
        private var dense: DenseKeyMap = null
        private var open: LongKeyMap   = null
        private var rows               = 0L

        def size: Int = if open != null then open.size else if dense != null then dense.size else 0

        def insert(keys: IndexedSeq[Array[?]], n: Int): Array[Int] =
            val values = Vectors.longs(keys(0))
            rows += n
            if open == null && n > 0 then
                var (lo, hi) = DenseKeyMap.range(values, n)
                if dense != null then
                    lo = math.min(lo, dense.min)
                    hi = math.max(hi, dense.max)
                if hi - lo < math.min(math.max(1L << 20, 4 * rows), MaxDenseRange) then
                    dense = if dense == null then DenseKeyMap(ints, lo, hi) else dense.widen(lo, hi)
                else
                    open = LongKeyMap(ints)
                    if dense != null then
                        val (sizeBefore, known) = (dense.size, dense.keys)
                        open.insert(known, sizeBefore)
                        dense = null
            if open != null then open.insert(values, n)
            else if dense != null then dense.insert(values, n)
            else new Array[Int](0)

        def find(keys: IndexedSeq[Array[?]], n: Int): Array[Int] =
            val values = Vectors.longs(keys(0))
            if open != null then open.find(values, n)
            else if dense != null then dense.find(values, n)
            else Vectors.minusOnes(n)

        def keyColumns: IndexedSeq[Array[?]] =
            if open != null then open.keyColumns else if dense != null then dense.keyColumns
            else IndexedSeq(if ints then Array.emptyIntArray else Array.emptyLongArray)
