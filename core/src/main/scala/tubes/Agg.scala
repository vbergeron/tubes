package tubes

import scala.annotation.implicitNotFound
import scala.quoted.*
import NamedTuple.{AnyNamedTuple, NamedTuple}

/** An aggregation over the rows of a group, written in a `groupBy` with `count` or `$.field.sum`-style calls. */
enum Agg[A]:
    case Count() extends Agg[Long]
    case Sum[A](col: Column[A], num: Numeric[A]) extends Agg[A]
    case Avg[A](col: Column[A], num: Numeric[A]) extends Agg[Double]
    case Min[A](col: Column[A], ord: Ordering[A]) extends Agg[A]
    case Max[A](col: Column[A], ord: Ordering[A]) extends Agg[A]
    case Alias[A, N <: String](agg: Agg[A], name: String) extends Agg[A], Named[N]

object Agg:
    extension [A](inline agg: Agg[A])
        /** Names this aggregation `name`, both in the AST and in its type. */
        inline def as[N <: String & Singleton](inline name: N): Agg[A] & Named[N] = Alias[A, N](agg, name)

/** The number of rows in the group. */
inline def count: Agg[Long] = Agg.Count()

/** How a tuple of named aggregations `G` becomes named output fields typed by `Out`. */
@implicitNotFound("Cannot aggregate ${G}: every aggregation must be named, with .as(\"name\")")
trait Aggregation[G]:
    type Out <: AnyNamedTuple
    def aggregates(g: G): List[Agg[?] & Named[?]]

object Aggregation:
    type Aux[G, O <: AnyNamedTuple] = Aggregation[G] { type Out = O }

    private def make[G, O <: AnyNamedTuple](f: G => List[Agg[?] & Named[?]]): Aux[G, O] =
        new Aggregation[G]:
            type Out = O
            def aggregates(g: G) = f(g)

    given single[N <: String, A]: Aux[Agg[A] & Named[N], NamedTuple[N *: EmptyTuple, A *: EmptyTuple]] = make(List(_))

    given empty: Aux[EmptyTuple, NamedTuple[EmptyTuple, EmptyTuple]] = make(_ => Nil)

    given cons[N <: String, A, T <: Tuple, ON <: Tuple, OV <: Tuple](using t: Aux[T, NamedTuple[ON, OV]])
        : Aux[(Agg[A] & Named[N]) *: T, NamedTuple[N *: ON, A *: OV]] =
        make(g => g.head :: t.aggregates(g.tail))

/** The running state of one aggregation in one group; each aggregation uses the slots it needs. */
final class Accumulator:
    var n: Long   = 0
    var l: Long   = 0
    var d: Double = 0
    var r: Any    = null

/** Aggregations compiled for a row kernel: `update` folds a row into a group's accumulators, `results` reads the
  * aggregated values out of them, in order.
  */
final class Aggregates(
    val size: Int,
    val update: (Array[Accumulator], Product) => Unit,
    val results: Array[Accumulator] => Array[Any]
)

/** Compiles the aggregations computed by a `groupBy` lambda. Sums, averages, minimums and maximums of `Int`, `Long` and
  * `Double` with their default instances accumulate in primitive slots; other types go through their instance.
  */
inline def compileAggregates[S, G](inline step: S ?=> G): Aggregates = ${ compileAggregatesImpl('step) }

private def compileAggregatesImpl[S: Type, G: Type](step: Expr[S ?=> G])(using Quotes): Expr[Aggregates] =
    import quotes.reflect.*
    val codes = tupleElements(stepBody(step).asTerm).zipWithIndex.map((agg, slot) => aggregate(agg, slot))
    '{
        Aggregates(
            ${ Expr(codes.size) },
            (accs: Array[Accumulator], row: Product) => ${ Expr.block(codes.map(_.update('accs, 'row)), '{ () }) },
            (accs: Array[Accumulator]) => Array[Any](${ Varargs(codes.map(_.result('accs))) }*)
        )
    }

/** How one aggregation updates its accumulator from a row, and reads its result back. */
private final class AggCode(
    val update: (Expr[Array[Accumulator]], Expr[Product]) => Quotes ?=> Expr[Unit],
    val result: Expr[Array[Accumulator]] => Quotes ?=> Expr[Any]
)

private def aggregate(using Quotes)(agg: Expr[Any], slot: Int): AggCode =
    import quotes.reflect.*
    val code = slotCode(slot)
    peel(agg.asTerm).asExpr match
        case '{ type n <: String; Agg.Alias[a, n]($inner, $name) } => aggregate(inner, slot)
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
