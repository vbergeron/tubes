package tubes

/** The loops of a key map, generated at a call site for keys of known types by `BatchKeyCompiler`: `columns` allocates
  * the arrays keeping the distinct keys, one per key column and of its type; `insert` and `find` number the keys of a
  * batch's key columns, as `KeyMap.insert` and `KeyMap.find` do, against a `KeyTable`.
  */
final class KeyCodec(
    val columns: () => Array[Array[?]],
    val insert: (KeyTable, IndexedSeq[Array[?]], Int) => Array[Int],
    val find: (KeyTable, IndexedSeq[Array[?]], Int) => Array[Int]
)

/** The table of a key map whose loops `codec` generates: an open-addressing table with linear probing, whose slots hold
  * key ids. Each id keeps its key's hash in `hashes` and its values in `stored`, one array per key column, holding the
  * column's values unboxed where its type is primitive. Lookups compare a row's values with an id's in place, typed, so
  * they box nothing.
  *
  * Its members are public for the generated loops, which run where the query is written; it is not meant to be used
  * otherwise.
  */
final class KeyTable(codec: KeyCodec):
    val stored: Array[Array[?]] = codec.columns()
    var slots: Array[Int]       = Vectors.minusOnes(KeyTable.capacity(16))
    var mask: Int               = slots.length - 1
    var hashes: Array[Int]      = new Array(16)
    var size: Int               = 0

    def insert(keys: IndexedSeq[Array[?]], n: Int): Array[Int] = codec.insert(this, keys, n)
    def find(keys: IndexedSeq[Array[?]], n: Int): Array[Int]   = codec.find(this, keys, n)

    /** Takes the next id for a key of hash `h`, making room for its values in `stored`; the caller stores them at that
      * id, then `place`s it.
      */
    def add(h: Int): Int =
        if size == hashes.length then
            hashes = java.util.Arrays.copyOf(hashes, size * 2)
            var c = 0
            while c < stored.length do
                stored(c) = KeyTable.resize(stored(c), size * 2)
                c += 1
        hashes(size) = h
        size += 1
        size - 1

    /** Puts key `id` in `slot`, doubling the table once it is half full. */
    def place(slot: Int, id: Int): Unit =
        slots(slot) = id
        if size * 2 > slots.length then grow()

    /** The key columns, holding each distinct key at its id; boxed values are unboxed where they all are primitives. */
    def keyColumns: IndexedSeq[Array[?]] = stored.toIndexedSeq.map:
        case a: Array[AnyRef] => Vectors.specialize(java.util.Arrays.copyOf(a, size))
        case a                => KeyTable.resize(a, size)

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

object KeyTable:
    /** Spreads the bits of `h` over an `Int`, so that close keys land far apart in a table (MurmurHash3's finalizer). */
    inline def mix(h: Long): Int =
        var x = h
        x = (x ^ (x >>> 33)) * 0xff51afd7ed558ccdL
        x = (x ^ (x >>> 33)) * 0xc4ceb9fe1a85ec53L
        (x ^ (x >>> 33)).toInt

    /** The smallest power of two above `2 * n`: tables stay at most half full. */
    def capacity(n: Int): Int = Integer.highestOneBit(math.max(8, n) * 2 - 1) * 2

    /** A copy of `a`, of the same array class, `length` long. */
    def resize(a: Array[?], length: Int): Array[?] =
        val out = java.lang.reflect.Array.newInstance(a.getClass.getComponentType, length)
        System.arraycopy(a, 0, out, 0, math.min(a.length, length))
        out.asInstanceOf[Array[?]]
