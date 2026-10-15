package tubes

import scala.annotation.implicitNotFound

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
    /** `lhs` and `rhs` hold `A` values, either of them possibly as an `Option[A]`, in which case `O` is optional too. */
    case Arith[A, L, R, O](op: ArithOp, lhs: Column[L], rhs: Column[R], num: Numeric[A]) extends Column[O]
    case Compare[A, L, R, O](op: CmpOp, lhs: Column[L], rhs: Column[R], ord: Ordering[A]) extends Column[O]
    case Equal[A](lhs: Column[A], rhs: Column[A]) extends Column[Boolean]
    case And(lhs: Column[Boolean], rhs: Column[Boolean]) extends Column[Boolean]
    case Or(lhs: Column[Boolean], rhs: Column[Boolean]) extends Column[Boolean]
    case Not(col: Column[Boolean]) extends Column[Boolean]
    case Default[A](col: Column[Option[A]], value: Column[A]) extends Column[A]
    case IsNone[A](col: Column[Option[A]]) extends Column[Boolean]

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

    /** The values an operator reads from a column of `A`s: `A` itself, or `x` from an optional `Option[x]`. */
    type Value[A] = A match
        case Option[x] => x
        case _         => A

    /** The type of an operator's `B` results on columns of `L` and `R`: optional when either side is. */
    type Lifted[L, R, B] = (L, R) match
        case (Option[?], ?) => Option[B]
        case (?, Option[?]) => Option[B]
        case _              => B

    /** Evidence that an operand of values `V` combines with a column of values `A`: they are the same type. */
    @implicitNotFound("Cannot combine ${V} values with a column of ${A} values")
    sealed trait Operand[V, A]
    object Operand:
        private object Same extends Operand[Any, Any]
        given same[A]: Operand[A, A] = Same.asInstanceOf[Operand[A, A]]

    private[tubes] inline def arith[L, R](inline op: ArithOp, inline lhs: Column[L], inline rhs: Column[R])(
        inline num: Numeric[Value[L]]
    ): Column[Lifted[L, R, Value[L]]] = Arith(op, lhs, rhs, num)

    private[tubes] inline def compare[L, R](inline op: CmpOp, inline lhs: Column[L], inline rhs: Column[R])(
        inline ord: Ordering[Value[L]]
    ): Column[Lifted[L, R, Boolean]] = Compare(op, lhs, rhs, ord)

    /** Arithmetic and comparisons ignore nullability: either side may be an `Option`, and so is the result then, `None`
      * where either side is. Comparisons with an optional side thus give an `Option[Boolean]`: `.default` it to filter.
      *
      * The right side is another column or a plain value, typed `R` or `V` rather than `Value[L]`: overloads are told
      * apart before `L` is known, when `Value[L]` cannot be reduced yet. So values are not widened: `1.0`, not `1`, with
      * a column of `Double`s.
      */
    extension [L](inline lhs: Column[L])(using num: Numeric[Value[L]])
        inline def +[R](inline rhs: Column[R])(using inline ev: Operand[Value[R], Value[L]]): Column[Lifted[L, R, Value[L]]] =
            arith(ArithOp.Plus, lhs, rhs)(num)
        inline def +[V](inline rhs: V)(using inline ev: Operand[V, Value[L]]): Column[Lifted[L, V, Value[L]]] =
            arith(ArithOp.Plus, lhs, Lit(rhs))(num)
        inline def -[R](inline rhs: Column[R])(using inline ev: Operand[Value[R], Value[L]]): Column[Lifted[L, R, Value[L]]] =
            arith(ArithOp.Minus, lhs, rhs)(num)
        inline def -[V](inline rhs: V)(using inline ev: Operand[V, Value[L]]): Column[Lifted[L, V, Value[L]]] =
            arith(ArithOp.Minus, lhs, Lit(rhs))(num)
        inline def *[R](inline rhs: Column[R])(using inline ev: Operand[Value[R], Value[L]]): Column[Lifted[L, R, Value[L]]] =
            arith(ArithOp.Times, lhs, rhs)(num)
        inline def *[V](inline rhs: V)(using inline ev: Operand[V, Value[L]]): Column[Lifted[L, V, Value[L]]] =
            arith(ArithOp.Times, lhs, Lit(rhs))(num)

    extension [L](inline lhs: Column[L])(using ord: Ordering[Value[L]])
        inline def <[R](inline rhs: Column[R])(using inline ev: Operand[Value[R], Value[L]]): Column[Lifted[L, R, Boolean]] =
            compare(CmpOp.Lt, lhs, rhs)(ord)
        inline def <[V](inline rhs: V)(using inline ev: Operand[V, Value[L]]): Column[Lifted[L, V, Boolean]] =
            compare(CmpOp.Lt, lhs, Lit(rhs))(ord)
        inline def <=[R](inline rhs: Column[R])(using inline ev: Operand[Value[R], Value[L]]): Column[Lifted[L, R, Boolean]] =
            compare(CmpOp.Le, lhs, rhs)(ord)
        inline def <=[V](inline rhs: V)(using inline ev: Operand[V, Value[L]]): Column[Lifted[L, V, Boolean]] =
            compare(CmpOp.Le, lhs, Lit(rhs))(ord)
        inline def >[R](inline rhs: Column[R])(using inline ev: Operand[Value[R], Value[L]]): Column[Lifted[L, R, Boolean]] =
            compare(CmpOp.Gt, lhs, rhs)(ord)
        inline def >[V](inline rhs: V)(using inline ev: Operand[V, Value[L]]): Column[Lifted[L, V, Boolean]] =
            compare(CmpOp.Gt, lhs, Lit(rhs))(ord)
        inline def >=[R](inline rhs: Column[R])(using inline ev: Operand[Value[R], Value[L]]): Column[Lifted[L, R, Boolean]] =
            compare(CmpOp.Ge, lhs, rhs)(ord)
        inline def >=[V](inline rhs: V)(using inline ev: Operand[V, Value[L]]): Column[Lifted[L, V, Boolean]] =
            compare(CmpOp.Ge, lhs, Lit(rhs))(ord)

    /** Optional columns, such as fields typed `Option[A]`, fall back to `value` where they hold `None`. */
    extension [A](inline lhs: Column[Option[A]])
        inline def default(inline value: Column[A]): Column[A] = Default(lhs, value)
        inline def default(inline value: A): Column[A]         = Default(lhs, Lit(value))

        /** Whether this column holds `None`, as SQL's `IS NULL`. */
        inline def isNone: Column[Boolean] = IsNone(lhs)

    extension (inline lhs: Column[Boolean])
        inline def &&(inline rhs: Column[Boolean]): Column[Boolean] = And(lhs, rhs)
        inline def ||(inline rhs: Column[Boolean]): Column[Boolean] = Or(lhs, rhs)
        inline def unary_! : Column[Boolean]                 = Not(lhs)

    trait Compiler[F[_]]:
        def compile[A](col: F[Column[A]]): F[Product => A]
