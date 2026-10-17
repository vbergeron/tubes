package tubes

/** Keys of one `Int` (`ints`) or `Long` column, in an open-addressing table with linear probing over primitive arrays:
  * slots hold the keys themselves next to their ids, so a lookup compares primitives and boxes nothing.
  */
private[tubes] final class LongKeyMap(ints: Boolean) extends KeyMap:
    private var slotKeys: Array[Long] = new Array(KeyMap.capacity(16))
    private var slotIds: Array[Int]   = Vectors.minusOnes(slotKeys.length)
    private var mask: Int             = slotKeys.length - 1
    private var keys: Array[Long]     = new Array(16)
    var size: Int                     = 0

    def insert(columns: IndexedSeq[Array[?]], n: Int): Array[Int] = insert(Vectors.longs(columns(0)), n)

    def find(columns: IndexedSeq[Array[?]], n: Int): Array[Int] = find(Vectors.longs(columns(0)), n)

    def insert(values: Array[Long], n: Int): Array[Int] =
        val ids = new Array[Int](n)
        var i   = 0
        while i < n do
            val k    = values(i)
            var slot = KeyMap.mix(k) & mask
            while slotIds(slot) >= 0 && slotKeys(slot) != k do slot = (slot + 1) & mask
            if slotIds(slot) >= 0 then ids(i) = slotIds(slot)
            else
                if size == keys.length then keys = java.util.Arrays.copyOf(keys, size * 2)
                keys(size) = k
                slotKeys(slot) = k
                slotIds(slot) = size
                ids(i) = size
                size += 1
                if size * 2 > slotIds.length then grow()
            i += 1
        ids

    def find(values: Array[Long], n: Int): Array[Int] =
        val ids = new Array[Int](n)
        var i   = 0
        while i < n do
            val k    = values(i)
            var slot = KeyMap.mix(k) & mask
            while slotIds(slot) >= 0 && slotKeys(slot) != k do slot = (slot + 1) & mask
            ids(i) = slotIds(slot)
            i += 1
        ids

    def keyColumns: IndexedSeq[Array[?]] =
        IndexedSeq(if ints then Vectors.build(size)(keys(_).toInt) else keys.take(size))

    /** Doubles the table, placing the keys again. */
    private def grow(): Unit =
        slotKeys = new Array(slotKeys.length * 2)
        slotIds = Vectors.minusOnes(slotKeys.length)
        mask = slotKeys.length - 1
        var id = 0
        while id < size do
            var slot = KeyMap.mix(keys(id)) & mask
            while slotIds(slot) >= 0 do slot = (slot + 1) & mask
            slotKeys(slot) = keys(id)
            slotIds(slot) = id
            id += 1
