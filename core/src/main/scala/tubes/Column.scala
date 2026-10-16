package tubes

import scala.compiletime.{error, summonFrom, summonInline}

enum ArithOp:
    case Plus, Minus, Times

enum CmpOp:
    case Lt, Le, Gt, Ge

/** Marks a column whose output name is statically known to be `N`. */
trait Named[N <: String]:
    def name: String

enum Column[A]:
    case Ref[A, N <: String](name: String, index: Int) extends Column[A], Named[N]
    case Alias[A, N <: String](col: Column[A], name: String) extends Column[A], Named[N]
    case Lit[A](value: A) extends Column[A]
    case Arith[A](op: ArithOp, lhs: Column[A], rhs: Column[A], num: Numeric[A]) extends Column[A]
    case Compare[A](op: CmpOp, lhs: Column[A], rhs: Column[A], ord: Ordering[A]) extends Column[Boolean]
    case Equal[A](lhs: Column[A], rhs: Column[A]) extends Column[Boolean]
    case And(lhs: Column[Boolean], rhs: Column[Boolean]) extends Column[Boolean]
    case Or(lhs: Column[Boolean], rhs: Column[Boolean]) extends Column[Boolean]
    case Not(col: Column[Boolean]) extends Column[Boolean]
    case Default[A](col: Column[Option[A]], value: Column[A]) extends Column[A]
    case Div[A](lhs: Column[A], rhs: Column[A], div: Fractional[A]) extends Column[A]
    case Quot[A](lhs: Column[A], rhs: Column[A], int: Integral[A]) extends Column[A]
    case ToDouble[A](col: Column[A], num: Numeric[A]) extends Column[Double]

    /** The value of `agg` over the rows of a group. Only `groupBy` computes these, as or within its output columns. */
    case Aggregate[A](agg: Agg[A]) extends Column[A]

object Column:
    /** The columns of a schema with field names `N` and field types `T`. */
    type NamedFields[N <: Tuple, T <: Tuple] <: Tuple = (N, T) match
        case (n *: ns, t *: ts)       => (Column[t] & Named[n & String]) *: NamedFields[ns, ts]
        case (EmptyTuple, EmptyTuple) => EmptyTuple

    inline def lit[A](inline value: A): Column[A] = Lit(value)

    extension [A](inline lhs: Column[A])
        inline def ===(inline rhs: Column[A]): Column[Boolean] = Equal(lhs, rhs)
        inline def ===(inline rhs: A): Column[Boolean]         = Equal(lhs, Lit(rhs))
        inline def =!=(inline rhs: Column[A]): Column[Boolean] = Not(Equal(lhs, rhs))
        inline def =!=(inline rhs: A): Column[Boolean]         = Not(Equal(lhs, Lit(rhs)))

        /** Names this column `name`, both in the AST and in its type. */
        inline def as[N <: String & Singleton](inline name: N): Column[A] & Named[N] = Alias[A, N](lhs, name)

    extension [A](inline lhs: Column[A])(using num: Numeric[A])
        inline def +(inline rhs: Column[A]): Column[A] = Arith(ArithOp.Plus, lhs, rhs, num)
        inline def +(inline rhs: A): Column[A]         = Arith(ArithOp.Plus, lhs, Lit(rhs), num)
        inline def -(inline rhs: Column[A]): Column[A] = Arith(ArithOp.Minus, lhs, rhs, num)
        inline def -(inline rhs: A): Column[A]         = Arith(ArithOp.Minus, lhs, Lit(rhs), num)
        inline def *(inline rhs: Column[A]): Column[A] = Arith(ArithOp.Times, lhs, rhs, num)
        inline def *(inline rhs: A): Column[A]         = Arith(ArithOp.Times, lhs, Lit(rhs), num)

    /** Division: exact for fractional types such as `Double`, truncating towards zero for integral ones such as `Int`. */
    extension [A](inline lhs: Column[A])
        inline def /(inline rhs: Column[A]): Column[A] = divide(lhs, rhs)
        inline def /(inline rhs: A): Column[A]         = divide(lhs, Lit(rhs))

    /** Picks the division of `A` where it is written, so the instance lands in the tree as is, like the others. */
    private inline def divide[A](inline lhs: Column[A], inline rhs: Column[A]): Column[A] = summonFrom:
        case _: Fractional[A] => Div(lhs, rhs, summonInline[Fractional[A]])
        case _: Integral[A]   => Quot(lhs, rhs, summonInline[Integral[A]])
        case _                => error("Cannot divide: no Fractional or Integral instance for the column type")

    extension [A](inline col: Column[A])(using num: Numeric[A])
        inline def toDouble: Column[Double] = ToDouble(col, num)

    extension [A](inline lhs: Column[A])(using ord: Ordering[A])
        inline def <(inline rhs: Column[A]): Column[Boolean]  = Compare(CmpOp.Lt, lhs, rhs, ord)
        inline def <(inline rhs: A): Column[Boolean]          = Compare(CmpOp.Lt, lhs, Lit(rhs), ord)
        inline def <=(inline rhs: Column[A]): Column[Boolean] = Compare(CmpOp.Le, lhs, rhs, ord)
        inline def <=(inline rhs: A): Column[Boolean]         = Compare(CmpOp.Le, lhs, Lit(rhs), ord)
        inline def >(inline rhs: Column[A]): Column[Boolean]  = Compare(CmpOp.Gt, lhs, rhs, ord)
        inline def >(inline rhs: A): Column[Boolean]          = Compare(CmpOp.Gt, lhs, Lit(rhs), ord)
        inline def >=(inline rhs: Column[A]): Column[Boolean] = Compare(CmpOp.Ge, lhs, rhs, ord)
        inline def >=(inline rhs: A): Column[Boolean]         = Compare(CmpOp.Ge, lhs, Lit(rhs), ord)

    /** Aggregations of a column over the rows of a group, for `groupBy`. They are columns themselves, so they combine
      * with any other column operation: `($.price.sum / $.qty.sum.toDouble).as("unitPrice")`.
      */
    extension [A](inline col: Column[A])
        inline def sum(using num: Numeric[A]): Column[A]      = Aggregate(Agg.Sum(col, num))
        inline def avg(using num: Numeric[A]): Column[Double] = Aggregate(Agg.Avg(col, num))
        inline def min(using ord: Ordering[A]): Column[A]     = Aggregate(Agg.Min(col, ord))
        inline def max(using ord: Ordering[A]): Column[A]     = Aggregate(Agg.Max(col, ord))

    /** Optional columns, such as fields typed `Option[A]`, fall back to `value` where they hold `None`. */
    extension [A](inline lhs: Column[Option[A]])
        inline def default(inline value: Column[A]): Column[A] = Default(lhs, value)
        inline def default(inline value: A): Column[A]         = Default(lhs, Lit(value))

    extension (inline lhs: Column[Boolean])
        inline def &&(inline rhs: Column[Boolean]): Column[Boolean] = And(lhs, rhs)
        inline def ||(inline rhs: Column[Boolean]): Column[Boolean] = Or(lhs, rhs)
        inline def unary_! : Column[Boolean]                 = Not(lhs)

    trait Compiler[F[_]]:
        def compile[A](col: F[Column[A]]): F[Product => A]
