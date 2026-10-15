package tubes

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

object Column:
    /** The columns of a schema with field names `N` and field types `T`. */
    type NamedFields[N <: Tuple, T <: Tuple] <: Tuple = (N, T) match
        case (n *: ns, t *: ts)       => (Column[t] & Named[n & String]) *: NamedFields[ns, ts]
        case (EmptyTuple, EmptyTuple) => EmptyTuple

    def lit[A](value: A): Column[A] = Lit(value)

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

    extension [A](inline lhs: Column[A])(using ord: Ordering[A])
        inline def <(inline rhs: Column[A]): Column[Boolean]  = Compare(CmpOp.Lt, lhs, rhs, ord)
        inline def <(inline rhs: A): Column[Boolean]          = Compare(CmpOp.Lt, lhs, Lit(rhs), ord)
        inline def <=(inline rhs: Column[A]): Column[Boolean] = Compare(CmpOp.Le, lhs, rhs, ord)
        inline def <=(inline rhs: A): Column[Boolean]         = Compare(CmpOp.Le, lhs, Lit(rhs), ord)
        inline def >(inline rhs: Column[A]): Column[Boolean]  = Compare(CmpOp.Gt, lhs, rhs, ord)
        inline def >(inline rhs: A): Column[Boolean]          = Compare(CmpOp.Gt, lhs, Lit(rhs), ord)
        inline def >=(inline rhs: Column[A]): Column[Boolean] = Compare(CmpOp.Ge, lhs, rhs, ord)
        inline def >=(inline rhs: A): Column[Boolean]         = Compare(CmpOp.Ge, lhs, Lit(rhs), ord)

    /** Aggregations of a column over the rows of a group, for `groupBy`. */
    extension [A](inline col: Column[A])
        inline def sum(using num: Numeric[A]): Agg[A]      = Agg.Sum(col, num)
        inline def avg(using num: Numeric[A]): Agg[Double] = Agg.Avg(col, num)
        inline def min(using ord: Ordering[A]): Agg[A]     = Agg.Min(col, ord)
        inline def max(using ord: Ordering[A]): Agg[A]     = Agg.Max(col, ord)

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
