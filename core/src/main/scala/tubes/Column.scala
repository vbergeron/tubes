package tubes

import scala.annotation.targetName

enum ArithOp:
    case Plus, Minus, Times

enum CmpOp:
    case Lt, Le, Gt, Ge

/** How an operation on `A` values producing `B` applies to `L` and `R` values, yielding `O`: when either side is an
  * `Option[A]`, so is the result an `Option[B]`, `None` where either side is.
  */
enum Lift[L, R, A, B, O]:
    case Plain[A, B]() extends Lift[A, A, A, B, B]
    case Left[A, B]()  extends Lift[Option[A], A, A, B, Option[B]]
    case Right[A, B]() extends Lift[A, Option[A], A, B, Option[B]]
    case Both[A, B]()  extends Lift[Option[A], Option[A], A, B, Option[B]]

/** Marks a column whose output name is statically known to be `N`. */
trait Named[N <: String]:
    def name: String

enum Column[A]:
    case Ref[A, N <: String](name: String, index: Int) extends Column[A], Named[N]
    case Alias[A, N <: String](col: Column[A], name: String) extends Column[A], Named[N]
    case Lit[A](value: A) extends Column[A]
    /** `lhs` and `rhs` hold `A` values, either of them possibly as an `Option[A]`, as `lift` tells. */
    case Arith[A, L, R, O](op: ArithOp, lhs: Column[L], rhs: Column[R], num: Numeric[A], lift: Lift[L, R, A, A, O])
        extends Column[O]
    case Compare[A, L, R, O](op: CmpOp, lhs: Column[L], rhs: Column[R], ord: Ordering[A], lift: Lift[L, R, A, Boolean, O])
        extends Column[O]
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

    /** Arithmetic and comparisons ignore nullability: either side may be an `Option`, and so is the result then, `None`
      * where either side is. Comparisons with an optional side thus give an `Option[Boolean]`: `.default` it to filter.
      *
      * Operators on plain and optional receivers live in separate given instances, so each receiver sees only its own
      * overloads: those on plain `Column[A]`s would otherwise also apply to optional ones, with `A` an `Option`.
      */
    final class PlainOps:
        extension [A](inline lhs: Column[A])(using num: Numeric[A])
            inline def +(inline rhs: Column[A]): Column[A] = Arith(ArithOp.Plus, lhs, rhs, num, Lift.Plain())
            inline def +(inline rhs: A): Column[A] = Arith(ArithOp.Plus, lhs, Lit(rhs), num, Lift.Plain())
            @targetName("plusRight")
            inline def +(inline rhs: Column[Option[A]]): Column[Option[A]] = Arith(ArithOp.Plus, lhs, rhs, num, Lift.Right())

            inline def -(inline rhs: Column[A]): Column[A] = Arith(ArithOp.Minus, lhs, rhs, num, Lift.Plain())
            inline def -(inline rhs: A): Column[A] = Arith(ArithOp.Minus, lhs, Lit(rhs), num, Lift.Plain())
            @targetName("minusRight")
            inline def -(inline rhs: Column[Option[A]]): Column[Option[A]] = Arith(ArithOp.Minus, lhs, rhs, num, Lift.Right())

            inline def *(inline rhs: Column[A]): Column[A] = Arith(ArithOp.Times, lhs, rhs, num, Lift.Plain())
            inline def *(inline rhs: A): Column[A] = Arith(ArithOp.Times, lhs, Lit(rhs), num, Lift.Plain())
            @targetName("timesRight")
            inline def *(inline rhs: Column[Option[A]]): Column[Option[A]] = Arith(ArithOp.Times, lhs, rhs, num, Lift.Right())


        extension [A](inline lhs: Column[A])(using ord: Ordering[A])
            inline def <(inline rhs: Column[A]): Column[Boolean] = Compare(CmpOp.Lt, lhs, rhs, ord, Lift.Plain())
            inline def <(inline rhs: A): Column[Boolean] = Compare(CmpOp.Lt, lhs, Lit(rhs), ord, Lift.Plain())
            @targetName("ltRight")
            inline def <(inline rhs: Column[Option[A]]): Column[Option[Boolean]] = Compare(CmpOp.Lt, lhs, rhs, ord, Lift.Right())

            inline def <=(inline rhs: Column[A]): Column[Boolean] = Compare(CmpOp.Le, lhs, rhs, ord, Lift.Plain())
            inline def <=(inline rhs: A): Column[Boolean] = Compare(CmpOp.Le, lhs, Lit(rhs), ord, Lift.Plain())
            @targetName("leRight")
            inline def <=(inline rhs: Column[Option[A]]): Column[Option[Boolean]] = Compare(CmpOp.Le, lhs, rhs, ord, Lift.Right())

            inline def >(inline rhs: Column[A]): Column[Boolean] = Compare(CmpOp.Gt, lhs, rhs, ord, Lift.Plain())
            inline def >(inline rhs: A): Column[Boolean] = Compare(CmpOp.Gt, lhs, Lit(rhs), ord, Lift.Plain())
            @targetName("gtRight")
            inline def >(inline rhs: Column[Option[A]]): Column[Option[Boolean]] = Compare(CmpOp.Gt, lhs, rhs, ord, Lift.Right())

            inline def >=(inline rhs: Column[A]): Column[Boolean] = Compare(CmpOp.Ge, lhs, rhs, ord, Lift.Plain())
            inline def >=(inline rhs: A): Column[Boolean] = Compare(CmpOp.Ge, lhs, Lit(rhs), ord, Lift.Plain())
            @targetName("geRight")
            inline def >=(inline rhs: Column[Option[A]]): Column[Option[Boolean]] = Compare(CmpOp.Ge, lhs, rhs, ord, Lift.Right())


    final class OptionOps:
        extension [A](inline lhs: Column[Option[A]])(using num: Numeric[A])
            @targetName("plusLeft")
            inline def +(inline rhs: Column[A]): Column[Option[A]] = Arith(ArithOp.Plus, lhs, rhs, num, Lift.Left())
            @targetName("plusLeftValue")
            inline def +(inline rhs: A): Column[Option[A]] = Arith(ArithOp.Plus, lhs, Lit(rhs), num, Lift.Left())
            @targetName("plusBoth")
            inline def +(inline rhs: Column[Option[A]]): Column[Option[A]] = Arith(ArithOp.Plus, lhs, rhs, num, Lift.Both())

            @targetName("minusLeft")
            inline def -(inline rhs: Column[A]): Column[Option[A]] = Arith(ArithOp.Minus, lhs, rhs, num, Lift.Left())
            @targetName("minusLeftValue")
            inline def -(inline rhs: A): Column[Option[A]] = Arith(ArithOp.Minus, lhs, Lit(rhs), num, Lift.Left())
            @targetName("minusBoth")
            inline def -(inline rhs: Column[Option[A]]): Column[Option[A]] = Arith(ArithOp.Minus, lhs, rhs, num, Lift.Both())

            @targetName("timesLeft")
            inline def *(inline rhs: Column[A]): Column[Option[A]] = Arith(ArithOp.Times, lhs, rhs, num, Lift.Left())
            @targetName("timesLeftValue")
            inline def *(inline rhs: A): Column[Option[A]] = Arith(ArithOp.Times, lhs, Lit(rhs), num, Lift.Left())
            @targetName("timesBoth")
            inline def *(inline rhs: Column[Option[A]]): Column[Option[A]] = Arith(ArithOp.Times, lhs, rhs, num, Lift.Both())


        extension [A](inline lhs: Column[Option[A]])(using ord: Ordering[A])
            @targetName("ltLeft")
            inline def <(inline rhs: Column[A]): Column[Option[Boolean]] = Compare(CmpOp.Lt, lhs, rhs, ord, Lift.Left())
            @targetName("ltLeftValue")
            inline def <(inline rhs: A): Column[Option[Boolean]] = Compare(CmpOp.Lt, lhs, Lit(rhs), ord, Lift.Left())
            @targetName("ltBoth")
            inline def <(inline rhs: Column[Option[A]]): Column[Option[Boolean]] = Compare(CmpOp.Lt, lhs, rhs, ord, Lift.Both())

            @targetName("leLeft")
            inline def <=(inline rhs: Column[A]): Column[Option[Boolean]] = Compare(CmpOp.Le, lhs, rhs, ord, Lift.Left())
            @targetName("leLeftValue")
            inline def <=(inline rhs: A): Column[Option[Boolean]] = Compare(CmpOp.Le, lhs, Lit(rhs), ord, Lift.Left())
            @targetName("leBoth")
            inline def <=(inline rhs: Column[Option[A]]): Column[Option[Boolean]] = Compare(CmpOp.Le, lhs, rhs, ord, Lift.Both())

            @targetName("gtLeft")
            inline def >(inline rhs: Column[A]): Column[Option[Boolean]] = Compare(CmpOp.Gt, lhs, rhs, ord, Lift.Left())
            @targetName("gtLeftValue")
            inline def >(inline rhs: A): Column[Option[Boolean]] = Compare(CmpOp.Gt, lhs, Lit(rhs), ord, Lift.Left())
            @targetName("gtBoth")
            inline def >(inline rhs: Column[Option[A]]): Column[Option[Boolean]] = Compare(CmpOp.Gt, lhs, rhs, ord, Lift.Both())

            @targetName("geLeft")
            inline def >=(inline rhs: Column[A]): Column[Option[Boolean]] = Compare(CmpOp.Ge, lhs, rhs, ord, Lift.Left())
            @targetName("geLeftValue")
            inline def >=(inline rhs: A): Column[Option[Boolean]] = Compare(CmpOp.Ge, lhs, Lit(rhs), ord, Lift.Left())
            @targetName("geBoth")
            inline def >=(inline rhs: Column[Option[A]]): Column[Option[Boolean]] = Compare(CmpOp.Ge, lhs, rhs, ord, Lift.Both())

    given plainOps: PlainOps   = PlainOps()
    given optionOps: OptionOps = OptionOps()

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
