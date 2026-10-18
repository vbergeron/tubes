package tubes

import scala.quoted.*
import BatchColumnCompiler.{Env, Repr, Unsupported, bind, isStandard, normalize, references, repr}

/** The aggregations of a `groupBy`, compiled for the batch kernel. Their running values live in a `FoldState` of
  * `longs`, `doubles`, `objects` and `seen` arrays indexed by group id. `update` folds a batch's rows into the groups
  * the ids give them, all aggregations in one loop; `results` computes the output columns from the running values of
  * the given number of groups, in order.
  */
final class FoldCode(
    val longs: Int,
    val doubles: Int,
    val objects: Int,
    val seen: Int,
    val update: (FoldState, Array[Int], ColumnBatch, Int) => Unit,
    val results: (FoldState, Int) => List[Array[?]]
)

/** The running values of compiled aggregations, one array per value kept, each indexed by group id. Public for the code
  * generated where a query is written; it is not meant to be used otherwise.
  */
final class FoldState(code: FoldCode):
    private var capacity = 16
    val longs: Array[Array[Long]]       = Array.fill(code.longs)(new Array[Long](capacity))
    val doubles: Array[Array[Double]]   = Array.fill(code.doubles)(new Array[Double](capacity))
    val objects: Array[Array[AnyRef]]   = Array.fill(code.objects)(new Array[AnyRef](capacity))
    val seen: Array[Array[Boolean]]     = Array.fill(code.seen)(new Array[Boolean](capacity))

    /** Makes room for `groups` groups, doubling the arrays as needed. */
    def ensure(groups: Int): Unit =
        if groups > capacity then
            capacity = math.max(groups, capacity * 2)
            for k <- longs.indices do longs(k) = java.util.Arrays.copyOf(longs(k), capacity)
            for k <- doubles.indices do doubles(k) = java.util.Arrays.copyOf(doubles(k), capacity)
            for k <- objects.indices do objects(k) = java.util.Arrays.copyOf(objects(k), capacity)
            for k <- seen.indices do seen(k) = java.util.Arrays.copyOf(seen(k), capacity)

    def update(gids: Array[Int], batch: ColumnBatch, groups: Int): Unit = code.update(this, gids, batch, groups)
    def results(groups: Int): List[Array[?]]                          = code.results(this, groups)

object FoldState:
    /** The first `n` values of `a`, as `Int`s: sums of `Int`s add up in `Long`s and are truncated at the end. */
    def ints(a: Array[Long], n: Int): Array[Int] = Vectors.build(n)(a(_).toInt)

    def longs(a: Array[Long], n: Int): Array[Long]       = java.util.Arrays.copyOf(a, n)
    def doubles(a: Array[Double], n: Int): Array[Double] = java.util.Arrays.copyOf(a, n)

    /** The first `n` values of `a`, unboxed where they all are primitives. */
    def objects(a: Array[AnyRef], n: Int): Array[?] = Vectors.specialize(java.util.Arrays.copyOf(a, n))

    /** The means of the first `n` groups, from their sums and counts. */
    def means(sums: Array[Double], counts: Array[Long], n: Int): Array[Double] = Vectors.build(n)(g => sums(g) / counts(g))

/** Compiles the output columns of a `groupBy` lambda for the batch kernel; `None` where an aggregation or an output
  * column holds a node the compiler does not handle, and the kernel then interprets them all.
  */
inline def compileBatchAggregates[S, G](inline step: S ?=> G): Option[FoldCode] = ${ compileBatchAggregatesImpl('step) }

private def compileBatchAggregatesImpl[S: Type, G: Type](step: Expr[S ?=> G])(using Quotes): Expr[Option[FoldCode]] =
    import quotes.reflect.*
    val outputs = tupleElements(stepBody(step).asTerm).map:
        case '{ $c: Column[t] } => c
        case other              => report.errorAndAbort(s"Not a column: ${other.show}", other)
    BatchColumnCompiler.attempt(BatchAggregateCompiler(outputs).code) match
        case Some(code) => '{ Some($code) }
        case None       => '{ None }

/** Generates the fold of the aggregations in `outputs`, and the computation of `outputs` from their values. Each
  * distinct aggregation keeps its running values in arrays of the state, as the batch kernel's accumulators do: sums,
  * averages, minimums and maximums of `Int`, `Long` and `Double` with their standard instances in primitive arrays,
  * others boxed, through their instances.
  */
private final class BatchAggregateCompiler(outputs: List[Expr[Column[?]]])(using Quotes):
    import quotes.reflect.*

    /** One aggregation: how a row updates its running values in a state, and how its output column is read from them.
      * `repr` is how that column holds its values.
      */
    private final class Slot(
        val agg: Expr[Agg[?]],
        val input: Option[Expr[Column[?]]],
        val update: (Expr[FoldState], Env, Expr[Int], Expr[Int]) => Expr[Unit],
        val result: (Expr[FoldState], Expr[Int]) => Expr[Array[?]],
        val repr: Repr
    )

    // How many arrays of each kind the state keeps, counted as slots take them.
    private var longs, doubles, objects, seen = 0
    private def longSlot(): Int   = { longs += 1; longs - 1 }
    private def doubleSlot(): Int = { doubles += 1; doubles - 1 }
    private def objectSlot(): Int = { objects += 1; objects - 1 }
    private def seenSlot(): Int   = { seen += 1; seen - 1 }

    /** The distinct aggregations in `outputs`, in order of first appearance; outer ones only, as the kernel computes. */
    private val aggregations: List[Expr[Agg[?]]] =
        def collect(col: Expr[Column[?]]): List[Expr[Agg[?]]] = stripColumn(col) match
            case '{ Column.Aggregate[t]($agg) } => List(agg)
            case c                              => children(c).getOrElse(Nil).flatMap(collect)
        outputs.flatMap(collect).foldLeft(List.empty[Expr[Agg[?]]]): (acc, a) =>
            if acc.exists(_.matches(a)) then acc else acc :+ a

    private val slots: List[Slot] = aggregations.map(slot)

    def code: Expr[FoldCode] =
        val update = '{ (state: FoldState, gids: Array[Int], batch: ColumnBatch, groups: Int) =>
            state.ensure(groups)
            ${ fold('state, 'gids, 'batch) }
        }
        val results = '{ (state: FoldState, groups: Int) => ${ columns('state, 'groups) } }
        '{ FoldCode(${ Expr(longs) }, ${ Expr(doubles) }, ${ Expr(objects) }, ${ Expr(seen) }, $update, $results) }

    /** One loop over the rows of `batch`, updating every slot of the group each row belongs to. */
    private def fold(state: Expr[FoldState], gids: Expr[Array[Int]], batch: Expr[ColumnBatch]): Expr[Unit] =
        val refs = slots.flatMap(_.input).flatMap(references).distinctBy(_._1)
        bind(refs.map((name, r) => (name, r, BatchColumnCompiler.load(name, r, batch)))): arrays =>
            val env = Env(arrays.toMap)
            '{
                val n = $batch.size
                var i = 0
                while i < n do
                    val row = i
                    val g   = $gids(row)
                    ${ Expr.block(slots.map(_.update(state, env, 'row, 'g)), '{ () }) }
                    i += 1
            }

    /** The output columns of `groups` groups: an aggregation's own values where an output is one, renamed or not, and a
      * loop over the groups computing the output from the aggregations' values otherwise.
      */
    private def columns(state: Expr[FoldState], groups: Expr[Int]): Expr[List[Array[?]]] =
        val values = slots.zipWithIndex.map((s, k) => (k, s.repr, normalize(s.result(state, groups), s.repr)))
        bind(values): bound =>
            val byAgg = slots.zip(bound.sortBy(_._1).map(_._2)).map((s, a) => (s.agg, a))
            val env   = Env(Map.empty, byAgg)
            Expr.ofList(outputs.map: col =>
                direct(col).flatMap(agg => byAgg.find(_._1.matches(agg))) match
                    case Some((_, values)) => values
                    case None =>
                        col match
                            case '{ $c: Column[t] } => BatchColumnCompiler.loop(c, env, groups))

    /** The aggregation `col` is, possibly renamed. */
    private def direct(col: Expr[Column[?]]): Option[Expr[Agg[?]]] = stripColumn(col) match
        case '{ type n <: String; Column.Alias[t, n]($c, $name) } => direct(c)
        case '{ Column.Aggregate[t]($agg) }                       => Some(agg)
        case _                                                    => None

    /** The value of `col` at `row`. */
    private def value[T: Type](col: Expr[Column[T]], env: Env, row: Expr[Int]): Expr[T] =
        BatchColumnCompiler.scalar(col, env, row)

    private def longArray(state: Expr[FoldState], k: Int)   = '{ $state.longs(${ Expr(k) }) }
    private def doubleArray(state: Expr[FoldState], k: Int) = '{ $state.doubles(${ Expr(k) }) }
    private def objectArray(state: Expr[FoldState], k: Int) = '{ $state.objects(${ Expr(k) }) }
    private def seenArray(state: Expr[FoldState], k: Int)   = '{ $state.seen(${ Expr(k) }) }

    private def slot(agg: Expr[Agg[?]]): Slot = peel(agg.asTerm).asExpr match
        case '{ Agg.Count() } =>
            val k = longSlot()
            Slot(agg, None, (st, _, _, g) => '{ ${ longArray(st, k) }($g) += 1 }, (st, n) => '{ FoldState.longs(${ longArray(st, k) }, $n) }, Repr.LongR)
        case '{ Agg.Sum[t]($col, $num) } =>
            repr[t] match
                case Repr.IntR if isStandard(num, BatchColumnCompiler.IntNumeric) =>
                    val k = longSlot()
                    Slot(
                        agg,
                        Some(col),
                        (st, env, row, g) => '{ ${ longArray(st, k) }($g) += ${ value(col, env, row).asExprOf[Int] } },
                        (st, n) => '{ FoldState.ints(${ longArray(st, k) }, $n) },
                        Repr.IntR
                    )
                case Repr.LongR if isStandard(num, BatchColumnCompiler.LongNumeric) =>
                    val k = longSlot()
                    Slot(
                        agg,
                        Some(col),
                        (st, env, row, g) => '{ ${ longArray(st, k) }($g) += ${ value(col, env, row).asExprOf[Long] } },
                        (st, n) => '{ FoldState.longs(${ longArray(st, k) }, $n) },
                        Repr.LongR
                    )
                case Repr.DoubleR if isStandard(num, BatchColumnCompiler.DoubleNumeric) =>
                    val k = doubleSlot()
                    Slot(
                        agg,
                        Some(col),
                        (st, env, row, g) => '{ ${ doubleArray(st, k) }($g) += ${ value(col, env, row).asExprOf[Double] } },
                        (st, n) => '{ FoldState.doubles(${ doubleArray(st, k) }, $n) },
                        Repr.DoubleR
                    )
                case r =>
                    val (k, s) = (objectSlot(), seenSlot())
                    Slot(
                        agg,
                        Some(col),
                        (st, env, row, g) =>
                            '{
                                val x    = ${ value(col, env, row) }
                                val kept = ${ objectArray(st, k) }
                                val seen = ${ seenArray(st, s) }
                                kept($g) = (if seen($g) then $num.plus(kept($g).asInstanceOf[t], x) else x).asInstanceOf[AnyRef]
                                seen($g) = true
                            },
                        (st, n) => '{ FoldState.objects(${ objectArray(st, k) }, $n) },
                        r
                    )
        case '{ Agg.Avg[t]($col, $num) } =>
            val (sums, counts) = (doubleSlot(), longSlot())
            def asDouble(env: Env, row: Expr[Int]): Expr[Double] = repr[t] match
                case Repr.IntR if isStandard(num, BatchColumnCompiler.IntNumeric) =>
                    '{ ${ value(col, env, row).asExprOf[Int] }.toDouble }
                case Repr.LongR if isStandard(num, BatchColumnCompiler.LongNumeric) =>
                    '{ ${ value(col, env, row).asExprOf[Long] }.toDouble }
                case Repr.DoubleR if isStandard(num, BatchColumnCompiler.DoubleNumeric) =>
                    value(col, env, row).asExprOf[Double]
                case _ => '{ $num.toDouble(${ value(col, env, row) }) }
            Slot(
                agg,
                Some(col),
                (st, env, row, g) =>
                    '{
                        ${ doubleArray(st, sums) }($g) += ${ asDouble(env, row) }
                        ${ longArray(st, counts) }($g) += 1
                    },
                (st, n) => '{ FoldState.means(${ doubleArray(st, sums) }, ${ longArray(st, counts) }, $n) },
                Repr.DoubleR
            )
        case '{ Agg.Min[t]($col, $ord) } => extremum(agg, col, ord, max = false)
        case '{ Agg.Max[t]($col, $ord) } => extremum(agg, col, ord, max = true)
        case _                           => throw Unsupported()

    /** A minimum, or a maximum when `max`: keeps each group's first value, then any value ordered before (after) it.
      * `Int`s and `Long`s with their standard orderings are kept as `Long`s, `Double`s with the standard total ordering
      * as `Double`s, other values boxed, compared through their ordering.
      */
    private def extremum[T: Type](agg: Expr[Agg[?]], col: Expr[Column[T]], ord: Expr[Ordering[T]], max: Boolean): Slot =
        /** Whether a comparison `c` of a new value with the kept one makes the new one replace it. */
        def replaces(c: Expr[Int]): Expr[Boolean] = if max then '{ $c > 0 } else '{ $c < 0 }
        val s = seenSlot()
        /** The update keeping values in the array `kept` gives, read and written as `P`, compared with `compare`. */
        def keep[P: Type](
            kept: Expr[FoldState] => Expr[Array[P]],
            read: (Env, Expr[Int]) => Expr[P],
            compare: (Expr[P], Expr[P]) => Expr[Int]
        ): (Expr[FoldState], Env, Expr[Int], Expr[Int]) => Expr[Unit] =
            (st, env, row, g) =>
                '{
                    val x    = ${ read(env, row) }
                    val seen = ${ seenArray(st, s) }
                    val vals = ${ kept(st) }
                    if !seen($g) || ${ replaces(compare('x, '{ vals($g) })) } then vals($g) = x
                    seen($g) = true
                }
        repr[T] match
            case Repr.IntR if isStandard(ord, BatchColumnCompiler.IntOrdering) =>
                val k = longSlot()
                Slot(
                    agg,
                    Some(col),
                    keep(longArray(_, k), (env, row) => '{ ${ value(col, env, row).asExprOf[Int] }.toLong }, (a, b) => '{ java.lang.Long.compare($a, $b) }),
                    (st, n) => '{ FoldState.ints(${ longArray(st, k) }, $n) },
                    Repr.IntR
                )
            case Repr.LongR if isStandard(ord, BatchColumnCompiler.LongOrdering) =>
                val k = longSlot()
                Slot(
                    agg,
                    Some(col),
                    keep(longArray(_, k), (env, row) => value(col, env, row).asExprOf[Long], (a, b) => '{ java.lang.Long.compare($a, $b) }),
                    (st, n) => '{ FoldState.longs(${ longArray(st, k) }, $n) },
                    Repr.LongR
                )
            case Repr.DoubleR if isStandard(ord, BatchColumnCompiler.DoubleTotalOrderings*) =>
                val k = doubleSlot()
                Slot(
                    agg,
                    Some(col),
                    keep(doubleArray(_, k), (env, row) => value(col, env, row).asExprOf[Double], (a, b) => '{ java.lang.Double.compare($a, $b) }),
                    (st, n) => '{ FoldState.doubles(${ doubleArray(st, k) }, $n) },
                    Repr.DoubleR
                )
            case r =>
                val k = objectSlot()
                Slot(
                    agg,
                    Some(col),
                    keep[AnyRef](
                        objectArray(_, k),
                        (env, row) => '{ ${ value(col, env, row) }.asInstanceOf[AnyRef] },
                        (a, b) => '{ $ord.compare($a.asInstanceOf[T], $b.asInstanceOf[T]) }
                    ),
                    (st, n) => '{ FoldState.objects(${ objectArray(st, k) }, $n) },
                    r
                )
