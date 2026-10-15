package tubes

import NamedTuple.AnyNamedTuple

/** Row-based kernel: streams a plan's rows through one iterator pipeline, using the row functions compiled into its
  * nodes, and collects the result.
  */
final class RowKernel[Out <: AnyNamedTuple] extends Plan.Compiler[Out]:
    type A = List[Out]

    def compile(plan: Plan[Out]): List[Out] =
        val result = RowKernel.rows(plan)
        (if RowKernel.yieldsTuples(plan) then result else result.map(Tuple.fromProduct)).toList.asInstanceOf[List[Out]]

object RowKernel:
    /** Rows produced by `plan`, in whatever representation its scan or last select left them. */
    private def rows(plan: Plan[?]): Iterator[Product] = plan match
        case s: Scan[?] =>
            s.source match
                case Some(src) => src.scan
                case None      => sys.error(s"RowKernel has no data bound for $s")
        case f: FilterSpec[?] => rows(f.input).filter(f.compiled)
        case s: SelectSpec[?] => rows(s.input).map(s.compiled)
        case j: JoinSpec[?]   => hashJoin(j)

    /** Whether `plan`'s rows are already `Out` tuples; otherwise they are still a source's own values. */
    private def yieldsTuples(plan: Plan[?]): Boolean = plan match
        case _: Scan[?]       => false
        case f: FilterSpec[?] => yieldsTuples(f.input)
        case _: SelectSpec[?] => true
        case _: JoinSpec[?]   => false

    /** Builds a hash table over the right rows, then streams the left rows through it, in order. Outer joins then add
      * the right rows that matched nothing.
      */
    private def hashJoin(j: JoinSpec[?]): Iterator[Product] =
        val table   = HashTable(rows(j.right), j.keys)
        val matched = if j.kind.keepsRight then new Array[Boolean](table.rows.length) else null
        val shape   = Shape(j.kind, j.left.schema.names.length, j.right.schema.names.length)
        val joined  = Probe(rows(j.left), table, j.keys, shape, matched)
        if matched == null then joined
        else
            joined ++ Iterator.range(0, table.rows.length).filterNot(matched).map(i => shape.row(null, table.rows(i)))

    /** How a join lays out its output rows. */
    private final class Shape(kind: JoinKind, val leftArity: Int, rightArity: Int):
        val inner: Boolean     = kind == JoinKind.Inner
        val keepsLeft: Boolean = kind.keepsLeft

        /** A row of `left` and `right`, either of which may be `null` where `kind` keeps unmatched rows. */
        def row(left: Product, right: Product): Product =
            if inner then JoinedRow(left, right, leftArity)
            else OuterRow(left, right, leftArity, rightArity, kind.keepsRight, kind.keepsLeft)

    /** Right rows grouped by key, in flat arrays. Each bucket chains its groups through `groupNext`, starting at
      * `heads`; each group chains its rows through `rowNext`, in input order, starting at `groupFirst`. `-1` ends a chain.
      * Keys are compared once per row while building, so probing compares each left row with one row per group only.
      */
    private final class HashTable(input: Iterator[Product], keys: HashKeys):
        val rows: Array[Product] = input.toArray
        val rowNext: Array[Int]  = Array.fill(rows.length)(-1)
        val mask: Int            = Integer.highestOneBit(math.max(1, rows.length) * 2 - 1) - 1
        val heads: Array[Int]    = Array.fill(mask + 1)(-1)

        // Groups, numbered in order of their first row; at most one per row.
        val groupFirst: Array[Int]   = new Array(rows.length)
        val groupHash: Array[Int]    = new Array(rows.length)
        val groupNext: Array[Int]    = new Array(rows.length)
        private val groupLast: Array[Int] = new Array(rows.length)
        private var groups               = 0

        private var i = 0
        while i < rows.length do
            val hash = spread(keys.rightHash(rows(i)))
            val b    = hash & mask
            var g    = heads(b)
            while g >= 0 && (groupHash(g) != hash || !keys.sameKey(rows(groupFirst(g)), rows(i))) do g = groupNext(g)
            if g >= 0 then
                rowNext(groupLast(g)) = i
                groupLast(g) = i
            else
                g = groups
                groups += 1
                groupFirst(g) = i
                groupLast(g) = i
                groupHash(g) = hash
                groupNext(g) = heads(b)
                heads(b) = g
            i += 1

        /** The first row of the group matching `left`, or `-1`. */
        def find(left: Product, hash: Int): Int =
            var g = heads(hash & mask)
            while g >= 0 && (groupHash(g) != hash || !keys.matches(left, rows(groupFirst(g)))) do g = groupNext(g)
            if g >= 0 then groupFirst(g) else -1

    private def spread(h: Int): Int = h ^ (h >>> 16)

    /** Pairs each left row with its matches in `table` that pass the residual, in order. Where the join keeps unmatched
      * left rows, emits those with no right side; where `matched` is given, marks the right rows it pairs.
      */
    private final class Probe(
        left: Iterator[Product],
        table: HashTable,
        keys: HashKeys,
        shape: Shape,
        matched: Array[Boolean]
    ) extends Iterator[Product]:
        private val residual  = keys.residual.orNull
        private val keepsLeft = shape.keepsLeft
        private var row: Product     = null
        private var at: Int          = -1
        private var found: Boolean   = false
        private var pending: Product = null

        def hasNext: Boolean =
            while pending == null && (row != null || left.hasNext) do advance()
            pending != null

        /** Moves to the next candidate pair, setting `pending` if it yields a row. */
        private def advance(): Unit =
            if row == null then
                row = left.next()
                at = table.find(row, spread(keys.leftHash(row)))
                found = false
            if at >= 0 then
                val i = at
                at = table.rowNext(at)
                val right  = table.rows(i)
                val joined = JoinedRow(row, right, shape.leftArity)
                if residual == null || residual(joined) then
                    found = true
                    if matched != null then matched(i) = true
                    pending = if shape.inner then joined else shape.row(row, right)
            else
                if keepsLeft && !found then pending = shape.row(row, null)
                row = null

        def next(): Product =
            if !hasNext then Iterator.empty.next()
            val out = pending
            pending = null
            out
