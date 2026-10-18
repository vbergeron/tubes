package tubes

import scala.collection.Factory
import scala.compiletime.erasedValue
import scala.reflect.ClassTag
import NamedTuple.AnyNamedTuple

/** `size` rows held column by column: `columns(name)` holds the values of field `name`, one per row. Each array is an
  * `Array[Int]`, `Array[Long]`, `Array[Double]` or `Array[Boolean]` when every value of the column has that type, and an
  * `Array[AnyRef]` of boxed values otherwise. Arrays are never modified once in a batch, so batches share them freely.
  */
final class ColumnBatch(val size: Int, val columns: Map[String, Array[?]]):
    def apply(name: String): Array[?] = columns(name)

    /** The rows at `indices(0 until count)`, in that order; only the `names` columns, or all of them. */
    def gather(indices: Array[Int], count: Int, names: Iterable[String] = columns.keys): ColumnBatch =
        ColumnBatch(count, names.iterator.map(n => n -> Vectors.gather(columns(n), indices, count)).toMap)

    /** The rows `from until until`. */
    def slice(from: Int, until: Int): ColumnBatch =
        ColumnBatch(until - from, columns.view.mapValues(Vectors.slice(_, from, until)).toMap)

    override def toString: String = columns.keys.mkString(s"ColumnBatch($size rows: ", ", ", ")")

/** Rows of `schema` stored column by column, in batches. The batch kernel reads them as they are; the row kernel reads
  * them as tuples.
  */
final class Columns[R <: AnyNamedTuple](val schema: Schema[R], val batches: IndexedSeq[ColumnBatch]):
    override def toString: String = s"Columns(${batches.map(_.size).sum} rows)"

object Columns:
    /** The case class values `rows`, stored column by column in batches of `batchSize`. */
    inline def from[C <: Product](rows: Seq[C], batchSize: Int = BatchKernel.DefaultBatchSize): Columns[NamedTuple.From[C]] =
        val schema = Schema.derived[C]
        Columns(schema, BatchKernel.transpose(rows.iterator, schema.names, batchSize).toIndexedSeq)

    given scanable[R <: AnyNamedTuple]: Scanable.Aux[Columns[R], R] =
        Scanable.make(_.schema, c => c.batches.iterator.flatMap(BatchKernel.rows(_, c.schema.names)))

/** Batch kernel: runs a plan as an iterator of `ColumnBatch`es of up to `batchSize` rows, evaluating each column
  * expression over whole columns at once, then collects the rows with `factory`. Plans read the same as with
  * `RowKernel`. Filters and selects run the batch functions compiled at their call site, one loop per column; the other
  * column trees kept in the nodes, and any a call site could not compile, are interpreted.
  * `Columns` sources are read in the batches they are stored in; other sources are cut into batches of `batchSize`.
  */
final class BatchKernel[Out <: AnyNamedTuple, C] private (factory: Factory[Out, C], batchSize: Int)
    extends Plan.Compiler[Out]:
    type A = C

    def compile(plan: Plan[Out]): C =
        val names = plan.schema.names
        val rows  = BatchKernel.batches(plan, batchSize).flatMap(BatchKernel.rows(_, names))
        factory.fromSpecific(rows.asInstanceOf[Iterator[Out]])

object BatchKernel:
    val DefaultBatchSize = 4096

    /** Collects the rows with a standard collection companion such as `List`, `Vector` or `Set`, or any `Factory`. */
    def apply[Out <: AnyNamedTuple, C](factory: Factory[Out, C], batchSize: Int = DefaultBatchSize): BatchKernel[Out, C] =
        new BatchKernel(factory, batchSize)

    /** Collects the rows into an `Array`. */
    def apply[Out <: AnyNamedTuple: ClassTag](factory: Array.type): BatchKernel[Out, Array[Out]] =
        new BatchKernel(Array.toFactory[Out](factory), DefaultBatchSize)

    /** Collects the rows into an `IArray`. */
    def apply[Out <: AnyNamedTuple: ClassTag](factory: IArray.type): BatchKernel[Out, IArray[Out]] =
        new BatchKernel(
            new Factory[Out, IArray[Out]]:
                def fromSpecific(it: IterableOnce[Out]): IArray[Out] = IArray.from(it)
                def newBuilder = Array.newBuilder[Out].mapResult(IArray.unsafeFromArray)
            ,
            DefaultBatchSize
        )

    /** The batches `plan` produces, holding the columns of its schema. Empty batches are skipped. */
    def batches(plan: Plan[?], batchSize: Int = DefaultBatchSize): Iterator[ColumnBatch] =
        batches(plan, batchSize, compiled = true)

    /** Same as `batches`; without `compiled`, interprets every column tree, to compare both ways. */
    private[tubes] def batches(plan: Plan[?], batchSize: Int, compiled: Boolean): Iterator[ColumnBatch] =
        require(batchSize > 0, s"batchSize must be positive, got $batchSize")
        run(plan, batchSize, compiled).filter(_.size > 0)

    /** The rows of `batch` as tuples of the `names` columns, in that order. */
    private[tubes] def rows(batch: ColumnBatch, names: IArray[String]): Iterator[Product] =
        val cols = names.toIndexedSeq.map(batch(_))
        Iterator.range(0, batch.size).map: i =>
            Tuple.fromArray(Vectors.build[Object](cols.length)(j => Vectors.get(cols(j), i).asInstanceOf[Object]))

    /** The batches of `plan`, computing filters and selects with their compiled batch functions when `compiled`. */
    private def run(plan: Plan[?], batchSize: Int, compiled: Boolean): Iterator[ColumnBatch] =
        /** The values of `col` over `batch`, through its compiled function `fn` if there is one. */
        def column(col: Column[?], fn: Option[BatchColumn], batch: ColumnBatch): Array[?] = fn match
            case Some(f) if compiled => f(batch)
            case _                   => Eval(col, batch)
        def input(p: Plan[?]) = run(p, batchSize, compiled)
        plan match
            case s: Scan[?] =>
                s.source match
                    case Some(src) =>
                        src.value match
                            case c: Columns[?] => c.batches.iterator
                            case _             => transpose(src.scan, s.schema.names, batchSize)
                    case None      => sys.error(s"BatchKernel has no data bound for $s")
            case f: FilterSpec[?] =>
                input(f.input).map: batch =>
                    val mask             = Vectors.booleans(column(f.predicate, f.batch, batch))
                    val (indices, count) = Vectors.where(mask, batch.size)
                    if count == batch.size then batch else batch.gather(indices, count)
            case s: SelectSpec[?] =>
                // Nodes built without batch functions select through the interpreter.
                val fns = if s.batch.isEmpty then s.columns.map(_ => None) else s.batch
                input(s.input).map: batch =>
                    ColumnBatch(batch.size, s.columns.zip(fns).map((c, fn) => c.name -> column(c, fn, batch)).toMap)
            case j: JoinSpec[?]    => HashJoin(j, input(j.left), input(j.right))
            case g: GroupBySpec[?] => Iterator.single(()).flatMap(_ => group(g, input(g.input), batchSize))
            case t: TakeSpec[?]    => take(input(t.input), t.count)
            case s: SkipSpec[?]    => skip(input(s.input), s.count)

    /** Cuts `rows` into batches of `batchSize`, one column per name. */
    private[tubes] def transpose(rows: Iterator[Product], names: IArray[String], batchSize: Int): Iterator[ColumnBatch] =
        rows.grouped(batchSize).map: group =>
            val n = group.size
            val columns = names.indices.map: j =>
                val values = new Array[AnyRef](n)
                var i      = 0
                for row <- group do
                    values(i) = row.productElement(j).asInstanceOf[AnyRef]
                    i += 1
                names(j) -> Vectors.specialize(values)
            ColumnBatch(n, columns.toMap)

    /** The first `n` rows; reads no further batch once it has them. */
    private def take(input: Iterator[ColumnBatch], n: Int): Iterator[ColumnBatch] = new Iterator[ColumnBatch]:
        private var left = n

        def hasNext: Boolean = left > 0 && input.hasNext

        def next(): ColumnBatch =
            val batch = input.next()
            val out   = if batch.size <= left then batch else batch.slice(0, left)
            left -= out.size
            out

    /** The rows after the first `n`. */
    private def skip(input: Iterator[ColumnBatch], n: Int): Iterator[ColumnBatch] =
        var left = math.max(n, 0)
        input.map: batch =>
            if left == 0 then batch
            else
                val dropped = math.min(left, batch.size)
                left -= dropped
                batch.slice(dropped, batch.size)

    /** Folds every input batch into the accumulators of its groups, then emits one row per group: its key columns, then
      * its aggregated columns. Groups come out in order of their first row, `batchSize` per batch.
      */
    private def group(g: GroupBySpec[?], input: Iterator[ColumnBatch], batchSize: Int): Iterator[ColumnBatch] =
        val aggs = g.aggregates.flatMap(Eval.aggregations).distinct
        // Aggregated values are read back as columns of the result batch, named after their slot.
        val slots      = aggs.zipWithIndex.map((agg, i) => (agg: Agg[?]) -> s"#agg$i").toMap[Agg[?], String]
        val accs = aggs.map(BatchAccumulator(_))
        // Group ids are key ids: groups are numbered in order of their first row.
        var groups: KeyMap = null
        for batch <- input do
            val keys = g.keys.map(Eval(_, batch)).toIndexedSeq
            if groups == null then groups = KeyMap(keys)
            val gids = groups.insert(keys, batch.size)
            accs.foreach(_.update(gids, batch, groups.size))
        if groups == null then groups = KeyMap(g.keys.map(_ => Array.empty[AnyRef]).toIndexedSeq)
        val count = groups.size
        val result = ColumnBatch(
            count,
            g.keys.zip(groups.keyColumns).map((k, values) => k.name -> values).toMap ++
                aggs.zip(accs).map((agg, acc) => slots(agg) -> acc.result(count))
        )
        val out = ColumnBatch(
            count,
            g.keys.map(k => k.name -> result(k.name)).toMap ++ g.aggregates.map(c => c.name -> Eval(c, result, slots))
        )
        Iterator.range(0, count, batchSize).map(from => out.slice(from, math.min(from + batchSize, count)))

/** Evaluates column trees over whole batches. */
private object Eval:
    import Column.*

    /** The values `col` takes on the rows of `batch`. Aggregations read their value from the column `aggs` names. */
    def apply(col: Column[?], batch: ColumnBatch, aggs: Map[Agg[?], String] = Map.empty): Array[?] =
        def eval(c: Column[?]): Array[?] = apply(c, batch, aggs)
        val n = batch.size
        col match
            case Ref(name, _)   => batch(name)
            case Alias(c, _)    => eval(c)
            case Lit(value)     => Vectors.fill(value, n)
            case Aggregate(agg) =>
                batch(aggs.getOrElse(agg, sys.error(s"$agg is only computed by groupBy")))
            case Arith(op, l, r, num) => Vectors.arith(op, eval(l), eval(r), num.asInstanceOf[Numeric[Any]], n)
            case Compare(op, l, r, ord) => Vectors.compare(op, eval(l), eval(r), ord.asInstanceOf[Ordering[Any]], n)
            case Equal(l, r) => Vectors.equal(eval(l), eval(r), n)
            // `&&` and `||` read their right side only on the rows the left side does not decide, as rows do: the right
            // side may fail elsewhere, e.g. dividing by a value the left side checks is not zero.
            case And(l, r) => logical(Vectors.booleans(eval(l)), r, batch, aggs, when = true)
            case Or(l, r)  => logical(Vectors.booleans(eval(l)), r, batch, aggs, when = false)
            case Not(c) =>
                val a = Vectors.booleans(eval(c))
                Vectors.build(n)(i => !a(i))
            case Default(c, value) =>
                val (a, v) = (eval(c), eval(value))
                Vectors.specialize(Vectors.build[AnyRef](n): i =>
                    Vectors.get(a, i) match
                        case Some(x) => x.asInstanceOf[AnyRef]
                        case _       => Vectors.get(v, i).asInstanceOf[AnyRef])
            case Div(l, r, frac) => Vectors.div(eval(l), eval(r), frac.asInstanceOf[Fractional[Any]], n)
            case Quot(l, r, int) => Vectors.quot(eval(l), eval(r), int.asInstanceOf[Integral[Any]], n)
            case ToDouble(c, num) => Vectors.toDouble(eval(c), num.asInstanceOf[Numeric[Any]], n)

    /** `left && right` when `when`, `left || right` otherwise: `right` is evaluated on the rows where `left` is `when`. */
    private def logical(left: Array[Boolean], right: Column[?], batch: ColumnBatch, aggs: Map[Agg[?], String], when: Boolean)
        : Array[Boolean] =
        val n                = batch.size
        val (indices, count) = Vectors.where(if when then left else left.map(!_), n)
        if count == 0 then left
        else if count == n then Vectors.booleans(apply(right, batch, aggs))
        else
            val rows = batch.gather(indices, count, refs(right, aggs))
            val r    = Vectors.booleans(apply(right, rows, aggs))
            val out  = left.clone()
            var k    = 0
            while k < count do
                out(indices(k)) = r(k)
                k += 1
            out

    /** The columns of a batch `col` reads, aggregated values included. */
    def refs(col: Column[?], aggs: Map[Agg[?], String] = Map.empty): Set[String] = col match
        case Ref(name, _)   => Set(name)
        case Aggregate(agg) => aggs.get(agg).toSet
        case c              => children(c).flatMap(refs(_, aggs)).toSet

    /** The aggregations `col` computes, outermost ones only. */
    def aggregations(col: Column[?]): List[Agg[?]] = col match
        case Aggregate(agg) => List(agg)
        case c              => children(c).flatMap(aggregations)

    /** The columns `col` is built from. */
    def children(col: Column[?]): List[Column[?]] = col match
        case Alias(c, _)          => List(c)
        case Arith(_, l, r, _)    => List(l, r)
        case Compare(_, l, r, _)  => List(l, r)
        case Equal(l, r)          => List(l, r)
        case And(l, r)            => List(l, r)
        case Or(l, r)             => List(l, r)
        case Not(c)               => List(c)
        case Default(c, v)        => List(c, v)
        case Div(l, r, _)         => List(l, r)
        case Quot(l, r, _)        => List(l, r)
        case ToDouble(c, _)       => List(c)
        case Ref(_, _) | Lit(_) | Aggregate(_) => Nil

/** One aggregation's running values for every group, in arrays indexed by group id. */
private abstract class BatchAccumulator:
    /** Folds the rows of `batch` into the groups `gids` gives them; `groups` groups exist so far. */
    def update(gids: Array[Int], batch: ColumnBatch, groups: Int): Unit

    /** The aggregated value of each of the `groups` groups. */
    def result(groups: Int): Array[?]

private object BatchAccumulator:
    def apply(agg: Agg[?]): BatchAccumulator = agg match
        case Agg.Count() =>
            new BatchAccumulator:
                private var counts = new Array[Long](16)
                def update(gids: Array[Int], batch: ColumnBatch, groups: Int): Unit =
                    counts = Vectors.grow(counts, groups)
                    var i = 0
                    while i < batch.size do
                        counts(gids(i)) += 1
                        i += 1
                def result(groups: Int): Array[?] = counts.take(groups)
        case Agg.Sum(col, num) => sum(col, num.asInstanceOf[Numeric[Any]])
        case Agg.Avg(col, num) =>
            new BatchAccumulator:
                private var sums   = new Array[Double](16)
                private var counts = new Array[Long](16)
                def update(gids: Array[Int], batch: ColumnBatch, groups: Int): Unit =
                    val values = Vectors.toDouble(Eval(col, batch), num.asInstanceOf[Numeric[Any]], batch.size)
                    sums = Vectors.grow(sums, groups)
                    counts = Vectors.grow(counts, groups)
                    var i = 0
                    while i < batch.size do
                        sums(gids(i)) += values(i)
                        counts(gids(i)) += 1
                        i += 1
                def result(groups: Int): Array[?] = Vectors.build(groups)(g => sums(g) / counts(g))
        case Agg.Min(col, ord) => extremum(col, ord.asInstanceOf[Ordering[Any]], max = false)
        case Agg.Max(col, ord) => extremum(col, ord.asInstanceOf[Ordering[Any]], max = true)

    /** Sums of `Int`, `Long` and `Double` with their default instances add up in primitive arrays; `Int` sums in
      * `Long`s, truncated at the end, as the row kernel does. Other sums go through `num`.
      */
    private def sum(col: Column[?], num: Numeric[Any]): BatchAccumulator = new BatchAccumulator:
        private var longs   = new Array[Long](16)
        private var doubles = new Array[Double](16)
        private var others  = new Array[AnyRef](16)
        private var seen    = new Array[Boolean](16)
        // How the sums are kept, settled on the first batch: 'i'nt or 'l'ong in `longs`, 'd'ouble, or 'o'ther values.
        private var kind = ' '

        def update(gids: Array[Int], batch: ColumnBatch, groups: Int): Unit =
            val values = Eval(col, batch)
            if kind == ' ' then
                kind = (values: Any) match
                    case _: Array[Int] if num eq Numeric.IntIsIntegral         => 'i'
                    case _: Array[Long] if num eq Numeric.LongIsIntegral       => 'l'
                    case _: Array[Double] if num eq Numeric.DoubleIsFractional => 'd'
                    case _                                                     => 'o'
            val n = batch.size
            kind match
                case 'i' =>
                    val a = Vectors.unbox[Int](values)
                    longs = Vectors.grow(longs, groups)
                    var i = 0
                    while i < n do
                        longs(gids(i)) += a(i)
                        i += 1
                case 'l' =>
                    val a = Vectors.unbox[Long](values)
                    longs = Vectors.grow(longs, groups)
                    var i = 0
                    while i < n do
                        longs(gids(i)) += a(i)
                        i += 1
                case 'd' =>
                    val a = Vectors.unbox[Double](values)
                    doubles = Vectors.grow(doubles, groups)
                    var i = 0
                    while i < n do
                        doubles(gids(i)) += a(i)
                        i += 1
                case _ =>
                    others = Vectors.grow(others, groups)
                    seen = Vectors.grow(seen, groups)
                    var i = 0
                    while i < n do
                        val g = gids(i)
                        val x = Vectors.get(values, i)
                        others(g) = (if seen(g) then num.plus(others(g), x) else x).asInstanceOf[AnyRef]
                        seen(g) = true
                        i += 1

        def result(groups: Int): Array[?] = kind match
            case 'i' => Vectors.build(groups)(g => longs(g).toInt)
            case 'l' => longs.take(groups)
            case 'd' => doubles.take(groups)
            case _   => Vectors.specialize(others.take(groups))

    /** A minimum, or a maximum when `max`: keeps each group's first value, then any value ordered before (after) it.
      * `Int`s, `Long`s and `Double`s with their default orderings are kept in primitive arrays, others through `ord`.
      */
    private def extremum(col: Column[?], ord: Ordering[Any], max: Boolean): BatchAccumulator = new BatchAccumulator:
        private var longs   = new Array[Long](16)
        private var doubles = new Array[Double](16)
        private var others  = new Array[AnyRef](16)
        private var seen    = new Array[Boolean](16)
        // How the values are kept, settled on the first batch: 'i'nt or 'l'ong in `longs`, 'd'ouble, or 'o'ther values.
        private var kind = ' '

        /** Whether `c`, the comparison of a new value with the kept one, makes the new value replace it. */
        private inline def replaces(c: Int): Boolean = if max then c > 0 else c < 0

        def update(gids: Array[Int], batch: ColumnBatch, groups: Int): Unit =
            val values = Eval(col, batch)
            if kind == ' ' then
                kind = (values: Any) match
                    case _: Array[Int] if ord eq Ordering.Int                                => 'i'
                    case _: Array[Long] if ord eq Ordering.Long                              => 'l'
                    case _: Array[Double] if ord.isInstanceOf[Ordering.Double.TotalOrdering] => 'd'
                    case _                                                                   => 'o'
            val n = batch.size
            seen = Vectors.grow(seen, groups)
            kind match
                case 'i' | 'l' =>
                    val a = if kind == 'i' then Vectors.unbox[Int](values).map(_.toLong) else Vectors.unbox[Long](values)
                    longs = Vectors.grow(longs, groups)
                    var i = 0
                    while i < n do
                        val g = gids(i)
                        if !seen(g) || replaces(java.lang.Long.compare(a(i), longs(g))) then longs(g) = a(i)
                        seen(g) = true
                        i += 1
                case 'd' =>
                    val a = Vectors.unbox[Double](values)
                    doubles = Vectors.grow(doubles, groups)
                    var i = 0
                    while i < n do
                        val g = gids(i)
                        if !seen(g) || replaces(java.lang.Double.compare(a(i), doubles(g))) then doubles(g) = a(i)
                        seen(g) = true
                        i += 1
                case _ =>
                    others = Vectors.grow(others, groups)
                    var i = 0
                    while i < n do
                        val g = gids(i)
                        val x = Vectors.get(values, i)
                        if !seen(g) || replaces(ord.compare(x, others(g))) then others(g) = x.asInstanceOf[AnyRef]
                        seen(g) = true
                        i += 1

        def result(groups: Int): Array[?] = kind match
            case 'i' => Vectors.build(groups)(g => longs(g).toInt)
            case 'l' => longs.take(groups)
            case 'd' => doubles.take(groups)
            case _   => Vectors.specialize(others.take(groups))

/** Pairs each left batch with the matching right rows, through a hash table over the whole right side built on the
  * first read. Equalities between a left and a right expression of the condition are the hash keys; the rest of it is
  * checked on the candidate pairs. Outer joins then add the right rows that matched nothing.
  */
private final class HashJoin(j: JoinSpec[?], left: Iterator[ColumnBatch], rightInput: Iterator[ColumnBatch])
    extends Iterator[ColumnBatch]:
    private val leftNames  = j.left.schema.names
    private val rightNames = j.right.schema.names
    private val (keys, residual) =
        val leftSet  = leftNames.toSet
        val rightSet = rightNames.toSet
        def side(c: Column[?]): Option[Boolean] =
            val refs = Eval.refs(c)
            if refs.nonEmpty && refs.subsetOf(leftSet) then Some(true)
            else if refs.nonEmpty && refs.subsetOf(rightSet) then Some(false)
            else None
        val parts = conjuncts(j.condition).map:
            case c @ Column.Equal(a, b) =>
                (side(a), side(b)) match
                    case (Some(true), Some(false)) => Left((a, b))
                    case (Some(false), Some(true)) => Left((b, a))
                    case _                         => Right(c)
            case c => Right(c)
        (parts.collect { case Left(k) => k }.toIndexedSeq, parts.collect { case Right(c) => c }.reduceOption(Column.And(_, _)))

    // Built on the first read: the right rows, the ids of their keys, and per key id the chain of its rows, in input
    // order: `first(id)` is its first row, `rowNext(row)` the next one, `-1` ends a chain.
    private lazy val right: ColumnBatch = Vectors.concat(rightInput.toList, rightNames)
    private lazy val (map, first, rowNext) =
        val rightKeys = keys.map((_, r) => Eval(r, right))
        val map       = KeyMap(rightKeys)
        val ids       = map.insert(rightKeys, right.size)
        val first     = Vectors.minusOnes(map.size)
        val last      = new Array[Int](map.size)
        val next      = Vectors.minusOnes(right.size)
        var row       = 0
        while row < right.size do
            val id = ids(row)
            if first(id) < 0 then first(id) = row else next(last(id)) = row
            last(id) = row
            row += 1
        (map, first, next)
    private lazy val matched: Array[Boolean] = if j.kind.keepsRight then new Array(right.size) else null
    private var unmatchedDone                = !j.kind.keepsRight

    def hasNext: Boolean = left.hasNext || !unmatchedDone

    def next(): ColumnBatch =
        if left.hasNext then probe(left.next())
        else if !unmatchedDone then
            unmatchedDone = true
            val rows = (0 until right.size).filterNot(matched).toArray
            output(null, Vectors.minusOnes(rows.length), rows, rows.length)
        else Iterator.empty.next()

    private def probe(batch: ColumnBatch): ColumnBatch =
        val leftKeys = keys.map((l, _) => Eval(l, batch))
        val ids      = map.find(leftKeys, batch.size)
        val (li, ri) = (Vectors.IntBuffer(), Vectors.IntBuffer())
        var i        = 0
        while i < batch.size do
            if ids(i) >= 0 then
                var r = first(ids(i))
                while r >= 0 do
                    li += i
                    ri += r
                    r = rowNext(r)
            i += 1
        val candidates = ColumnBatch(li.size, batch.gather(li.array, li.size).columns ++ right.gather(ri.array, ri.size).columns)
        val pass       = residual.map(c => Vectors.booleans(Eval(c, candidates)))
        if j.kind == JoinKind.Inner then
            pass.fold(candidates): mask =>
                val (indices, count) = Vectors.where(mask, candidates.size)
                candidates.gather(indices, count)
        else
            // Keeps the passing pairs, in left order, and the left rows without one where the join keeps them.
            val (outLeft, outRight) = (Vectors.IntBuffer(), Vectors.IntBuffer())
            var c                   = 0
            var row                 = 0
            while row < batch.size do
                var found = false
                while c < li.size && li(c) == row do
                    if pass.forall(_(c)) then
                        found = true
                        outLeft += row
                        outRight += ri(c)
                        if matched != null then matched(ri(c)) = true
                    c += 1
                if !found && j.kind.keepsLeft then
                    outLeft += row
                    outRight += -1
                row += 1
            output(batch, outLeft.array, outRight.array, outLeft.size)

    /** The joined rows of `batch` row `leftRows(k)` and right row `rightRows(k)`, `-1` where a side is missing; the fields
      * of a side an outer join may leave missing are optional.
      */
    private def output(batch: ColumnBatch, leftRows: Array[Int], rightRows: Array[Int], count: Int): ColumnBatch =
        val optionalLeft = j.kind.keepsRight
        val optionalRight = j.kind.keepsLeft
        val l = leftNames.map(n => n -> Vectors.gatherOptional(if batch == null then null else batch(n), leftRows, count, optionalLeft))
        val r = rightNames.map(n => n -> Vectors.gatherOptional(right(n), rightRows, count, optionalRight))
        ColumnBatch(count, (l ++ r).toMap)

    private def conjuncts(c: Column[Boolean]): List[Column[Boolean]] = c match
        case Column.And(l, r) => conjuncts(l) ++ conjuncts(r)
        case other            => List(other)

/** Operations over the columns of a batch, with loops over primitive arrays where the types and instances allow. */
private object Vectors:
    /** An array of the `n` values `f(0)`, ..., `f(n - 1)`. Inlined, its loop reads and writes primitives where `T` is
      * one, where `Array.tabulate` boxes every value.
      */
    inline def build[T](n: Int)(inline f: Int => T): Array[T] =
        val out = allocate[T](n)
        var i   = 0
        while i < n do
            out(i) = f(i)
            i += 1
        out

    /** A new array of `n` `T`s, of the array class picked at compile time. `new Array[T](n)` would go through its
      * `ClassTag` at run time, which allocates slowly once the JIT has seen several element types at the same call.
      */
    inline def allocate[T](n: Int): Array[T] = inline erasedValue[T] match
        case _: Int     => new Array[Int](n).asInstanceOf[Array[T]]
        case _: Long    => new Array[Long](n).asInstanceOf[Array[T]]
        case _: Double  => new Array[Double](n).asInstanceOf[Array[T]]
        case _: Boolean => new Array[Boolean](n).asInstanceOf[Array[T]]
        case _: AnyRef  => new Array[AnyRef](n).asInstanceOf[Array[T]]

    /** `n` `-1`s, the end of every chain and the mark of every empty slot. */
    def minusOnes(n: Int): Array[Int] =
        val out = new Array[Int](n)
        java.util.Arrays.fill(out, -1)
        out

    /** Value `i` of `a`, boxed. */
    def get(a: Array[?], i: Int): Any = (a: Any) match
        case a: Array[Int]     => a(i)
        case a: Array[Long]    => a(i)
        case a: Array[Double]  => a(i)
        case a: Array[Boolean] => a(i)
        case a: Array[AnyRef]  => a(i)

    /** `values`, unboxed into a primitive array when they all are `Int`s, `Long`s, `Double`s or `Boolean`s. */
    def specialize(values: Array[AnyRef]): Array[?] =
        val n = values.length
        if n == 0 then values
        else
            def all(cls: Class[?]): Boolean = values.forall(v => v != null && v.getClass == cls)
            values(0) match
                case _: java.lang.Integer if all(classOf[java.lang.Integer]) => build(n)(values(_).asInstanceOf[Int])
                case _: java.lang.Long if all(classOf[java.lang.Long])       => build(n)(values(_).asInstanceOf[Long])
                case _: java.lang.Double if all(classOf[java.lang.Double])   => build(n)(values(_).asInstanceOf[Double])
                case _: java.lang.Boolean if all(classOf[java.lang.Boolean]) => build(n)(values(_).asInstanceOf[Boolean])
                case _                                                       => values

    /** `value`, `n` times. */
    def fill(value: Any, n: Int): Array[?] = value match
        case v: Int     => build(n)(_ => v)
        case v: Long    => build(n)(_ => v)
        case v: Double  => build(n)(_ => v)
        case v: Boolean => build(n)(_ => v)
        case v          => build[AnyRef](n)(_ => v.asInstanceOf[AnyRef])

    /** `a` as booleans, unboxing it if needed. */
    def booleans(a: Array[?]): Array[Boolean] = unbox[Boolean](a)

    /** `a` as an array of the primitive `T`, unboxing it if it is not one already. */
    def unbox[T: ClassTag](a: Array[?]): Array[T] =
        if a.getClass.getComponentType == summon[ClassTag[T]].runtimeClass then a.asInstanceOf[Array[T]]
        else
            val out = new Array[T](a.length)
            for i <- 0 until a.length do out(i) = get(a, i).asInstanceOf[T]
            out

    /** The positions where `mask(0 until n)` holds, and how many there are. */
    def where(mask: Array[Boolean], n: Int): (Array[Int], Int) =
        val out   = new Array[Int](n)
        var count = 0
        var i     = 0
        while i < n do
            if mask(i) then
                out(count) = i
                count += 1
            i += 1
        (out, count)

    def gather(a: Array[?], indices: Array[Int], n: Int): Array[?] = (a: Any) match
        case a: Array[Int]     => build(n)(k => a(indices(k)))
        case a: Array[Long]    => build(n)(k => a(indices(k)))
        case a: Array[Double]  => build(n)(k => a(indices(k)))
        case a: Array[Boolean] => build(n)(k => a(indices(k)))
        case a: Array[AnyRef]  => build[AnyRef](n)(k => a(indices(k)))

    /** Same as `gather`, where index `-1` is a missing row; when `optional`, values are wrapped in `Some`, `None` where
      * missing. `a` may be `null` when every index is `-1`.
      */
    def gatherOptional(a: Array[?], indices: Array[Int], n: Int, optional: Boolean): Array[?] =
        if !optional then gather(a, indices, n)
        else build[AnyRef](n)(k => if indices(k) < 0 then None else Some(get(a, indices(k))))

    def slice(a: Array[?], from: Int, until: Int): Array[?] = (a: Any) match
        case a: Array[Int]     => java.util.Arrays.copyOfRange(a, from, until)
        case a: Array[Long]    => java.util.Arrays.copyOfRange(a, from, until)
        case a: Array[Double]  => java.util.Arrays.copyOfRange(a, from, until)
        case a: Array[Boolean] => java.util.Arrays.copyOfRange(a, from, until)
        case a: Array[AnyRef]  => java.util.Arrays.copyOfRange(a, from, until)

    /** The rows of `batches` in one batch with the `names` columns. */
    def concat(batches: List[ColumnBatch], names: IArray[String]): ColumnBatch =
        batches match
            case List(single) => single
            case _ =>
                val size = batches.map(_.size).sum
                ColumnBatch(
                    size,
                    names.map: name =>
                        val parts = batches.map(_(name))
                        // Parts of one array type are copied as they are; others are boxed into one array.
                        if parts.nonEmpty && parts.forall(_.getClass == parts.head.getClass) then
                            val values = java.lang.reflect.Array.newInstance(parts.head.getClass.getComponentType, size)
                            var at     = 0
                            for part <- parts do
                                System.arraycopy(part, 0, values, at, part.length)
                                at += part.length
                            name -> values.asInstanceOf[Array[?]]
                        else
                            val values = new Array[AnyRef](size)
                            var at     = 0
                            for part <- parts; i <- 0 until part.length do
                                values(at) = get(part, i).asInstanceOf[AnyRef]
                                at += 1
                            name -> specialize(values)
                    .toMap
                )

    /** `a` as `Long`s: its `Int`s widened, or its boxed integers unboxed. */
    def longs(a: Array[?]): Array[Long] = (a: Any) match
        case a: Array[Long] => a
        case a: Array[Int]  => build(a.length)(a(_).toLong)
        case _              => build(a.length)(get(a, _).asInstanceOf[Number].longValue)

    /** `a`, grown so that index `size - 1` exists. */
    def grow[T: ClassTag](a: Array[T], size: Int): Array[T] =
        if size <= a.length then a
        else
            val out = new Array[T](math.max(size, a.length * 2))
            System.arraycopy(a, 0, out, 0, a.length)
            out

    def arith(op: ArithOp, l: Array[?], r: Array[?], num: Numeric[Any], n: Int): Array[?] = ((l, r): Any) match
        case (a: Array[Int], b: Array[Int]) if num eq Numeric.IntIsIntegral =>
            op match
                case ArithOp.Plus  => build(n)(i => a(i) + b(i))
                case ArithOp.Minus => build(n)(i => a(i) - b(i))
                case ArithOp.Times => build(n)(i => a(i) * b(i))
        case (a: Array[Long], b: Array[Long]) if num eq Numeric.LongIsIntegral =>
            op match
                case ArithOp.Plus  => build(n)(i => a(i) + b(i))
                case ArithOp.Minus => build(n)(i => a(i) - b(i))
                case ArithOp.Times => build(n)(i => a(i) * b(i))
        case (a: Array[Double], b: Array[Double]) if num eq Numeric.DoubleIsFractional =>
            op match
                case ArithOp.Plus  => build(n)(i => a(i) + b(i))
                case ArithOp.Minus => build(n)(i => a(i) - b(i))
                case ArithOp.Times => build(n)(i => a(i) * b(i))
        case _ =>
            val f: (Any, Any) => Any = op match
                case ArithOp.Plus  => num.plus
                case ArithOp.Minus => num.minus
                case ArithOp.Times => num.times
            specialize(build[AnyRef](n)(i => f(get(l, i), get(r, i)).asInstanceOf[AnyRef]))

    def compare(op: CmpOp, l: Array[?], r: Array[?], ord: Ordering[Any], n: Int): Array[Boolean] =
        val test: Int => Boolean = op match
            case CmpOp.Lt => _ < 0
            case CmpOp.Le => _ <= 0
            case CmpOp.Gt => _ > 0
            case CmpOp.Ge => _ >= 0
        ((l, r): Any) match
            case (a: Array[Int], b: Array[Int]) if ord eq Ordering.Int =>
                build(n)(i => test(Integer.compare(a(i), b(i))))
            case (a: Array[Long], b: Array[Long]) if ord eq Ordering.Long =>
                build(n)(i => test(java.lang.Long.compare(a(i), b(i))))
            case _ =>
                // Through `lt`, `lteq`, `gt` and `gteq`, as rows are: an ordering may define them apart from `compare`.
                val f: (Any, Any) => Boolean = op match
                    case CmpOp.Lt => ord.lt
                    case CmpOp.Le => ord.lteq
                    case CmpOp.Gt => ord.gt
                    case CmpOp.Ge => ord.gteq
                build(n)(i => f(get(l, i), get(r, i)))

    def equal(l: Array[?], r: Array[?], n: Int): Array[Boolean] = ((l, r): Any) match
        case (a: Array[Int], b: Array[Int])         => build(n)(i => a(i) == b(i))
        case (a: Array[Long], b: Array[Long])       => build(n)(i => a(i) == b(i))
        case (a: Array[Double], b: Array[Double])   => build(n)(i => a(i) == b(i))
        case (a: Array[Boolean], b: Array[Boolean]) => build(n)(i => a(i) == b(i))
        case _                                      => build(n)(i => get(l, i) == get(r, i))

    def div(l: Array[?], r: Array[?], frac: Fractional[Any], n: Int): Array[?] = ((l, r): Any) match
        case (a: Array[Double], b: Array[Double]) if frac eq Numeric.DoubleIsFractional => build(n)(i => a(i) / b(i))
        case _ => specialize(build[AnyRef](n)(i => frac.div(get(l, i), get(r, i)).asInstanceOf[AnyRef]))

    def quot(l: Array[?], r: Array[?], int: Integral[Any], n: Int): Array[?] = ((l, r): Any) match
        case (a: Array[Int], b: Array[Int]) if int eq Numeric.IntIsIntegral    => build(n)(i => a(i) / b(i))
        case (a: Array[Long], b: Array[Long]) if int eq Numeric.LongIsIntegral => build(n)(i => a(i) / b(i))
        case _ => specialize(build[AnyRef](n)(i => int.quot(get(l, i), get(r, i)).asInstanceOf[AnyRef]))

    def toDouble(a: Array[?], num: Numeric[Any], n: Int): Array[Double] = (a: Any) match
        case a: Array[Int] if num eq Numeric.IntIsIntegral         => build(n)(a(_).toDouble)
        case a: Array[Long] if num eq Numeric.LongIsIntegral       => build(n)(a(_).toDouble)
        case a: Array[Double] if num eq Numeric.DoubleIsFractional => a
        case _                                                     => build(n)(i => num.toDouble(get(a, i)))

    /** A growable array of `Int`s. */
    final class IntBuffer:
        var array: Array[Int] = new Array(16)
        var size: Int         = 0

        def +=(x: Int): Unit =
            if size == array.length then array = java.util.Arrays.copyOf(array, size * 2)
            array(size) = x
            size += 1

        def apply(i: Int): Int = array(i)
