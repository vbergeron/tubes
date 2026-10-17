package tubes

/** Keys of one `Int` (`ints`) or `Long` column within `min to max`, by direct addressing: key `k` has its id at
  * `ids(k - min)`. No hashing and no comparison, at the cost of one `Int` per value of the range.
  */
private[tubes] final class DenseKeyMap(ints: Boolean, val min: Long, val max: Long) extends KeyMap:
    private var ids: Array[Int] = Vectors.minusOnes((max - min + 1).toInt)
    private[tubes] var keys: Array[Long] = new Array(16)
    var size: Int                        = 0

    def insert(columns: IndexedSeq[Array[?]], n: Int): Array[Int] = insert(Vectors.longs(columns(0)), n)

    def find(columns: IndexedSeq[Array[?]], n: Int): Array[Int] = find(Vectors.longs(columns(0)), n)

    /** The ids of the first `n` `values`, which must lie within `min to max`. */
    def insert(values: Array[Long], n: Int): Array[Int] =
        val out = new Array[Int](n)
        var i   = 0
        while i < n do
            val at = (values(i) - min).toInt
            var id = ids(at)
            if id < 0 then
                if size == keys.length then keys = java.util.Arrays.copyOf(keys, size * 2)
                keys(size) = values(i)
                id = size
                ids(at) = id
                size += 1
            out(i) = id
            i += 1
        out

    def find(values: Array[Long], n: Int): Array[Int] =
        val out = new Array[Int](n)
        var i   = 0
        while i < n do
            val k = values(i)
            out(i) = if k < min || k > max then -1 else ids((k - min).toInt)
            i += 1
        out

    def keyColumns: IndexedSeq[Array[?]] =
        IndexedSeq(if ints then Vectors.build(size)(keys(_).toInt) else keys.take(size))

    /** The same keys and ids, within the wider range `lo to hi`. */
    def widen(lo: Long, hi: Long): DenseKeyMap =
        if lo == min && hi == max then this
        else
            val wider = DenseKeyMap(ints, lo, hi)
            System.arraycopy(ids, 0, wider.ids, (min - lo).toInt, ids.length)
            wider.keys = keys
            wider.size = size
            wider

private[tubes] object DenseKeyMap:
    /** The smallest and largest of the first `n` `values`; `(0, 0)` when there are none. */
    def range(values: Array[Long], n: Int): (Long, Long) =
        if n == 0 then (0L, 0L)
        else
            var (lo, hi) = (values(0), values(0))
            var i        = 1
            while i < n do
                if values(i) < lo then lo = values(i)
                if values(i) > hi then hi = values(i)
                i += 1
            (lo, hi)
