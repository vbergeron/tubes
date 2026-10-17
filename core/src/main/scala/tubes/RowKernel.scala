package tubes

import scala.collection.Factory
import scala.jdk.CollectionConverters.*
import scala.reflect.ClassTag
import NamedTuple.AnyNamedTuple

/** Row-based kernel: streams a plan's rows through one iterator pipeline, using the row functions compiled into its
  * nodes, and hands them to `collect`, which may hold off running the plan until its rows are read.
  */
final class RowKernel[Out <: AnyNamedTuple, C] private (collect: (=> Iterator[Out]) => C) extends Plan.Compiler[Out]:
    type A = C

    def compile(plan: Plan[Out]): C =
        collect(RowKernel.rows(plan, asTuples = true).asInstanceOf[Iterator[Out]])

object RowKernel:
    /** Collects the rows with a standard collection companion such as `List`, `Vector` or `Set`, or any `Factory`. */
    def apply[Out <: AnyNamedTuple, C](factory: Factory[Out, C]): RowKernel[Out, C] =
        new RowKernel(rows => factory.fromSpecific(rows))

    /** Collects the rows into an `Array`. */
    def apply[Out <: AnyNamedTuple: ClassTag](factory: Array.type): RowKernel[Out, Array[Out]] =
        new RowKernel(rows => rows.toArray)

    /** Collects the rows into an `IArray`. */
    def apply[Out <: AnyNamedTuple: ClassTag](factory: IArray.type): RowKernel[Out, IArray[Out]] =
        new RowKernel(rows => IArray.from(rows))

    /** Streams the rows into a `LazyList`. The plan starts running when its first row is read, and computes rows as
      * they are read, except for what a `groupBy` or a join needs up front: all the rows it groups, or its right rows.
      */
    def apply[Out <: AnyNamedTuple](factory: LazyList.type): RowKernel[Out, LazyList[Out]] =
        new RowKernel(rows => LazyList.empty.lazyAppendedAll(rows))

    /** Rows produced by `plan`. Inside the plan rows keep whatever representation is cheapest, such as a source's own
      * values or joined rows read in place; `asTuples` asks for `Out` tuples instead, which the node producing them
      * builds directly rather than having them copied afterwards.
      */
    private def rows(plan: Plan[?], asTuples: Boolean = false): Iterator[Product] = plan match
        case s: Scan[?] =>
            s.source match
                case Some(src) => if asTuples then src.scan.map(Tuple.fromProduct) else src.scan
                case None      => sys.error(s"RowKernel has no data bound for $s")
        case f: FilterSpec[?]  => rows(f.input, asTuples).filter(f.compiled)
        case s: SelectSpec[?]  => rows(s.input).map(s.compiled)
        case j: JoinSpec[?]    => hashJoin(j, asTuples)
        case g: GroupBySpec[?] => group(g)
        case t: TakeSpec[?]    => rows(t.input, asTuples).take(t.count)
        case s: SkipSpec[?]    => rows(s.input, asTuples).drop(s.count)

    /** Folds every input row into the accumulators of its group, then emits one row per group: its key fields, then
      * its aggregated values. Groups come out in order of their first row.
      */
    private def group(g: GroupBySpec[?]): Iterator[Product] =
        val aggs   = g.compiled
        val groups = java.util.LinkedHashMap[Any, Array[Accumulator]]()
        val input  = rows(g.input)
        while input.hasNext do
            val row  = input.next()
            val key  = g.key(row)
            var accs = groups.get(key)
            if accs == null then
                accs = Array.fill(aggs.slots)(Accumulator())
                groups.put(key, accs)
            aggs.update(accs, row)
        val arity = g.keys.size
        groups.entrySet.iterator.asScala.map: entry =>
            val out = new Array[Object](arity + aggs.size)
            if arity == 1 then out(0) = entry.getKey.asInstanceOf[Object]
            else
                val key = entry.getKey.asInstanceOf[Product]
                var i   = 0
                while i < arity do
                    out(i) = key.productElement(i).asInstanceOf[Object]
                    i += 1
            val results = aggs.results(entry.getValue)
            var j       = 0
            while j < results.length do
                out(arity + j) = results(j).asInstanceOf[Object]
                j += 1
            Tuple.fromArray(out)

    /** Builds a hash table over the right rows, then streams the left rows through it, in order. Outer joins then add
      * the right rows that matched nothing.
      */
    private def hashJoin(j: JoinSpec[?], asTuples: Boolean): Iterator[Product] =
        val table   = HashTable(rows(j.right), j.keys)
        val matched = if j.kind.keepsRight then new Array[Boolean](table.rows.length) else null
        val shape   = Shape(j.kind, j.left.schema.names.length, j.right.schema.names.length, asTuples)
        val joined  = Probe(rows(j.left), table, j.keys, shape, matched)
        if matched == null then joined
        else
            joined ++ Iterator.range(0, table.rows.length).filterNot(matched).map(i => shape.row(null, table.rows(i)))

    /** How a join lays out its output rows: read in place as joined rows, or built once as `Out` tuples when
      * `asTuples`, so the result is not copied again.
      */
    private final class Shape(kind: JoinKind, val leftArity: Int, rightArity: Int, val asTuples: Boolean):
        val inner: Boolean     = kind == JoinKind.Inner
        val keepsLeft: Boolean = kind.keepsLeft

        /** One view reused for every output tuple: `Tuple.fromProduct` copies its fields straight into the tuple. */
        private val view = RowView(leftArity, rightArity, kind.keepsRight, kind.keepsLeft)

        /** A row of `left` and `right`, either of which may be `null` where `kind` keeps unmatched rows. */
        def row(left: Product, right: Product): Product =
            if asTuples then
                view.left = left
                view.right = right
                Tuple.fromProduct(view)
            else if inner then JoinedRow(left, right, leftArity)
            else OuterRow(left, right, leftArity, rightArity, kind.keepsRight, kind.keepsLeft)

    /** The fields of `left` then `right`, read in place like an `OuterRow`, but pointed at a new pair for each row. */
    private final class RowView(leftArity: Int, rightArity: Int, optionalLeft: Boolean, optionalRight: Boolean)
        extends Product:
        var left: Product  = null
        var right: Product = null

        def productArity: Int = leftArity + rightArity

        def productElement(n: Int): Any =
            if n < leftArity then field(left, n, optionalLeft) else field(right, n - leftArity, optionalRight)

        private def field(side: Product, n: Int, optional: Boolean): Any =
            if !optional then side.productElement(n)
            else if side == null then None
            else Some(side.productElement(n))

        def canEqual(that: Any): Boolean = false

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
                val right = table.rows(i)
                // The residual reads the pair as a joined row; without one, no row is built for rejected candidates.
                val joined = if residual == null then null else JoinedRow(row, right, shape.leftArity)
                if joined == null || residual(joined) then
                    found = true
                    if matched != null then matched(i) = true
                    pending = if joined != null && shape.inner && !shape.asTuples then joined else shape.row(row, right)
            else
                if keepsLeft && !found then pending = shape.row(row, null)
                row = null

        def next(): Product =
            if !hasNext then Iterator.empty.next()
            val out = pending
            pending = null
            out
