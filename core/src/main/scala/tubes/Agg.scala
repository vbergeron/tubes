package tubes

import scala.quoted.*

/** An aggregation over the rows of a group. Aggregations are written as columns, with `count` or `$.field.sum`-style
  * calls that wrap them in `Column.Aggregate`, so they combine with the rest of the column algebra.
  */
enum Agg[A]:
    case Count() extends Agg[Long]
    case Sum[A](col: Column[A], num: Numeric[A]) extends Agg[A]
    case Avg[A](col: Column[A], num: Numeric[A]) extends Agg[Double]
    case Min[A](col: Column[A], ord: Ordering[A]) extends Agg[A]
    case Max[A](col: Column[A], ord: Ordering[A]) extends Agg[A]

/** The number of rows in the group. */
inline def count: Column[Long] = Column.Aggregate(Agg.Count())

/** The running state of one aggregation in one group; each aggregation uses the slots it needs. */
final class Accumulator:
    var n: Long   = 0
    var l: Long   = 0
    var d: Double = 0
    var r: Any    = null

/** Aggregations compiled for a row kernel: each group keeps `slots` accumulators, `update` folds a row into them, and
  * `results` computes the group's `size` output values from them, in order.
  */
final class Aggregates(
    val slots: Int,
    val size: Int,
    val update: (Array[Accumulator], Product) => Unit,
    val results: Array[Accumulator] => Array[Any]
)

/** Compiles the output columns of a `groupBy` lambda. Each distinct aggregation they contain gets an accumulator slot;
  * the columns are then computed from the aggregated values, as if those were the fields of a row. Sums, averages,
  * minimums and maximums of `Int`, `Long` and `Double` with their default instances accumulate in primitive slots;
  * other types go through their instance.
  */
inline def compileAggregates[S, G](inline step: S ?=> G): Aggregates = ${ compileAggregatesImpl('step) }

private def compileAggregatesImpl[S: Type, G: Type](step: Expr[S ?=> G])(using Quotes): Expr[Aggregates] =
    import quotes.reflect.*
    val outputs = tupleElements(stepBody(step).asTerm).map:
        case '{ $c: Column[t] } => c
        case other              => report.errorAndAbort(s"Not a column: ${other.show}", other)
    val aggs = scala.collection.mutable.ArrayBuffer.empty[Expr[Agg[?]]]
    /** The slot of `agg`, shared by every occurrence of the same aggregation. */
    def slotOf(agg: Expr[Agg[?]]): Int =
        val found = aggs.indexWhere(_.matches(agg))
        if found >= 0 then found else { aggs += agg; aggs.size - 1 }
    val computed = outputs.map(c => directSlot(c).map(slotOf).toLeft(overAggregates(c, slotOf)))
    val codes    = aggs.toList.zipWithIndex.map((agg, slot) => aggregate(agg, slot))
    /** The output values, given the aggregated values; composite columns read them through a row. */
    def results(values: Expr[Array[Any]])(using Quotes): Expr[Array[Any]] =
        if computed == List.tabulate(codes.size)(Left(_)) then values
        else
            val row = '{ ArrayRow($values) }
            val out = computed.map:
                case Left(slot) => '{ $values(${ Expr(slot) }) }
                case Right(col) =>
                    val f: Expr[Product => Any] = QuotedColumnCompiler.compile(col.asInstanceOf[Expr[Column[Any]]])
                    Expr.betaReduce('{ $f($row) })
            '{ Array[Any](${ Varargs(out) }*) }
    '{
        Aggregates(
            ${ Expr(codes.size) },
            ${ Expr(outputs.size) },
            (accs: Array[Accumulator], row: Product) => ${ Expr.block(codes.map(_.update('accs, 'row)), '{ () }) },
            (accs: Array[Accumulator]) =>
                val values = Array[Any](${ Varargs(codes.map(_.result('accs))) }*)
                ${ results('values) }
        )
    }

/** The aggregation `col` computes as it is, possibly renamed; `None` when it computes more, or something else. */
private def directSlot(using Quotes)(col: Expr[Column[?]]): Option[Expr[Agg[?]]] =
    stripColumn(col) match
        case '{ type n <: String; Column.Alias[a, n]($c, $name) } => directSlot(c)
        case '{ Column.Aggregate[a]($agg) }                         => Some(agg)
        case _                                                      => None

/** `col` with each aggregation in it replaced by a reference to its value, in field `slotOf(aggregation)` of the row of
  * aggregated values. Fields of the input rows cannot be read there, outside of an aggregation.
  */
private def overAggregates(using Quotes)(col: Expr[Column[?]], slotOf: Expr[Agg[?]] => Int): Expr[Column[?]] =
    import quotes.reflect.*
    val mapper = new TreeMap:
        override def transformTerm(tree: Term)(owner: Symbol): Term =
            if !tree.isExpr || !(tree.tpe <:< TypeRepr.of[Column[?]]) then super.transformTerm(tree)(owner)
            else
                tree.asExpr match
                    case '{ Column.Aggregate[a]($agg) } =>
                        '{ Column.Ref[a, String]("aggregate", ${ Expr(slotOf(agg)) }) }.asTerm
                    case '{ type n <: String; Column.Ref[a, n]($name, $index) } =>
                        report.errorAndAbort(
                            s"Column ${name.valueOrAbort} must be aggregated, with sum, avg, min or max, to be used in a groupBy aggregation",
                            tree.pos
                        )
                    case _ => super.transformTerm(tree)(owner)
    mapper.transformTerm(col.asTerm)(Symbol.spliceOwner).asExprOf[Column[?]]

/** A row whose fields are `values`, read in place. */
final class ArrayRow(values: Array[Any]) extends Product:
    def productArity: Int            = values.length
    def productElement(n: Int): Any  = values(n)
    def canEqual(that: Any): Boolean = false

/** How one aggregation updates its accumulator from a row, and reads its result back. */
private final class AggCode(
    val update: (Expr[Array[Accumulator]], Expr[Product]) => Quotes ?=> Expr[Unit],
    val result: Expr[Array[Accumulator]] => Quotes ?=> Expr[Any]
)

private def aggregate(using Quotes)(agg: Expr[Any], slot: Int): AggCode =
    import quotes.reflect.*
    val code = slotCode(slot)
    peel(agg.asTerm).asExpr match
        case '{ Agg.Count() } =>
            code((a, _) => '{ $a.n += 1 }, a => '{ $a.n })
        case '{ Agg.Sum[t]($col, $num) } =>
            Type.of[t] match
                case '[Int] if isDefault(num) =>
                    code((a, row) => '{ $a.l += ${ valueOf(col, row).asExprOf[Int] } }, a => '{ $a.l.toInt })
                case '[Long] if isDefault(num) =>
                    code((a, row) => '{ $a.l += ${ valueOf(col, row).asExprOf[Long] } }, a => '{ $a.l })
                case '[Double] if isDefault(num) =>
                    code((a, row) => '{ $a.d += ${ valueOf(col, row).asExprOf[Double] } }, a => '{ $a.d })
                case _ =>
                    code(
                        (a, row) =>
                            '{
                                val x = ${ valueOf(col, row) }
                                $a.r = if $a.n == 0 then x else $num.plus($a.r.asInstanceOf[t], x)
                                $a.n += 1
                            },
                        a => '{ $a.r }
                    )
        case '{ Agg.Avg[t]($col, $num) } =>
            val toDouble: Expr[Product] => Quotes ?=> Expr[Double] = Type.of[t] match
                case '[Int] if isDefault(num)    => row => '{ ${ valueOf(col, row).asExprOf[Int] }.toDouble }
                case '[Long] if isDefault(num)   => row => '{ ${ valueOf(col, row).asExprOf[Long] }.toDouble }
                case '[Double] if isDefault(num) => row => valueOf(col, row).asExprOf[Double]
                case _                           => row => '{ $num.toDouble(${ valueOf(col, row) }) }
            code((a, row) => '{ $a.d += ${ toDouble(row) }; $a.n += 1 }, a => '{ $a.d / $a.n })
        case '{ Agg.Min[t]($col, $ord) } => extremum(col, ord, slot, max = false)
        case '{ Agg.Max[t]($col, $ord) } => extremum(col, ord, slot, max = true)
        case other => report.errorAndAbort(s"Cannot compile aggregation ${other.show}", other)

/** Builds the code of the aggregation in accumulator `slot` from how it updates and reads one accumulator. */
private def slotCode(slot: Int)(using Quotes)(
    update: (Expr[Accumulator], Expr[Product]) => Quotes ?=> Expr[Unit],
    result: Expr[Accumulator] => Quotes ?=> Expr[Any]
): AggCode =
    AggCode(
        (accs, row) => '{ val a = $accs(${ Expr(slot) }); ${ update('a, row) } },
        accs => result('{ $accs(${ Expr(slot) }) })
    )

/** The value `col` takes on `row`, compiled inline. */
private def valueOf[T: Type](using Quotes)(col: Expr[Column[T]], row: Expr[Product]): Expr[T] =
    val f = QuotedColumnCompiler.compile(col)
    Expr.betaReduce('{ $f($row) })

/** A minimum, or a maximum when `max`: keeps the first value, then any value ordered before (after) the kept one. */
private def extremum[T: Type](using Quotes)(col: Expr[Column[T]], ord: Expr[Ordering[T]], slot: Int, max: Boolean)
    : AggCode =
    val code = slotCode(slot)
    /** Whether the new value `x` replaces the kept one, given their comparison `c` (negative when `x` comes first). */
    def better(c: Expr[Int])(using Quotes): Expr[Boolean] = if max then '{ $c > 0 } else '{ $c < 0 }
    Type.of[T] match
        case '[Int] if isDefault(ord) =>
            code(
                (a, row) =>
                    '{
                        val x = ${ valueOf(col, row).asExprOf[Int] }
                        if $a.n == 0 || ${ better('{ java.lang.Long.compare(x, $a.l) }) } then $a.l = x
                        $a.n += 1
                    },
                a => '{ $a.l.toInt }
            )
        case '[Long] if isDefault(ord) =>
            code(
                (a, row) =>
                    '{
                        val x = ${ valueOf(col, row).asExprOf[Long] }
                        if $a.n == 0 || ${ better('{ java.lang.Long.compare(x, $a.l) }) } then $a.l = x
                        $a.n += 1
                    },
                a => '{ $a.l }
            )
        case '[Double] if isDefault(ord) =>
            code(
                (a, row) =>
                    '{
                        val x = ${ valueOf(col, row).asExprOf[Double] }
                        if $a.n == 0 || ${ better('{ java.lang.Double.compare(x, $a.d) }) } then $a.d = x
                        $a.n += 1
                    },
                a => '{ $a.d }
            )
        case _ =>
            code(
                (a, row) =>
                    '{
                        val x = ${ valueOf(col, row) }
                        if $a.n == 0 || ${ better('{ $ord.compare(x, $a.r.asInstanceOf[T]) }) } then $a.r = x
                        $a.n += 1
                    },
                a => '{ $a.r }
            )

/** Whether `instance` is the given instance of `X` found by implicit search, e.g. `Numeric.IntIsIntegral`. */
private def isDefault[X: Type](using Quotes)(instance: Expr[X]): Boolean =
    Expr.summon[X].exists(_.matches(instance))
