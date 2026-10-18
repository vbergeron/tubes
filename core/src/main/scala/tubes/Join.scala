package tubes

import scala.quoted.*
import NamedTuple.{AnyNamedTuple, DropNames, Names}

/** The right side of a join, seen from the joined row: field access yields references past the `L` fields. */
final class RightSchema[L <: AnyNamedTuple, R <: AnyNamedTuple](val schema: Schema[R], val offset: Int) extends Selectable:
    type Fields = NamedTuple.NamedTuple[Names[R], Column.NamedFields[Names[R], DropNames[R]]]

    /** Resolved at compile time to `Column.Ref[A, "foo"]("foo", arity(L) + index)` when `L` and `R` are known. */
    transparent inline def selectDynamic(inline name: String): Column[?] = ${ RightSchema.refImpl[L, R]('this, 'name) }

    /** Runtime lookup, for schemas whose type is not known statically. */
    def ref(name: String): Column[?] = Column.Ref[Any, String](name, offset + schema.names.indexOf(name))

object RightSchema:
    def refImpl[L <: AnyNamedTuple: Type, R <: AnyNamedTuple: Type](self: Expr[RightSchema[L, R]], name: Expr[String])(
        using Quotes
    ): Expr[Column[?]] =
        Schema.arity[L].flatMap(Schema.staticRef[R](name, _)).getOrElse('{ $self.ref($name) })

/** Inside a join condition, `$left` and `$right` are the schemas of the two joined inputs. Both refer to columns of the
  * joined row, which holds the left fields followed by the right ones.
  */
final class JoinSchemas[L <: AnyNamedTuple, R <: AnyNamedTuple](val left: Schema[L], val right: RightSchema[L, R])

inline transparent def `$left`(using schemas: JoinSchemas[?, ?]): schemas.left.type   = schemas.left
inline transparent def `$right`(using schemas: JoinSchemas[?, ?]): schemas.right.type = schemas.right

/** A row of a join: the fields of `left`, then those of `right`, read in place without copying. */
final class JoinedRow(val left: Product, val right: Product, leftArity: Int) extends Product:
    def productArity: Int = leftArity + right.productArity

    def productElement(n: Int): Any =
        if n < leftArity then left.productElement(n) else right.productElement(n - leftArity)

    def canEqual(that: Any): Boolean = false

    override def toString: String = productIterator.mkString("JoinedRow(", ", ", ")")

/** A row of an outer join: the fields of `left`, then those of `right`, read in place. Fields of an optional side are
  * wrapped in `Some`, or `None` where that side is missing (`null`).
  */
final class OuterRow(
    val left: Product,
    val right: Product,
    leftArity: Int,
    rightArity: Int,
    optionalLeft: Boolean,
    optionalRight: Boolean
) extends Product:
    def productArity: Int = leftArity + rightArity

    def productElement(n: Int): Any =
        if n < leftArity then OuterRow.field(left, n, optionalLeft)
        else OuterRow.field(right, n - leftArity, optionalRight)

    def canEqual(that: Any): Boolean = false

    override def toString: String = productIterator.mkString("OuterRow(", ", ", ")")

object OuterRow:
    private def field(side: Product, n: Int, optional: Boolean): Any =
        if !optional then side.productElement(n)
        else if side == null then None
        else Some(side.productElement(n))

/** A join condition compiled for a hash join: equalities between a left and a right expression become the hash keys.
  * `leftHash` and `rightHash` hash the keys of left and right rows alike, `matches` compares a left key with a right one,
  * `sameKey` two right ones, and `residual` checks whatever else the condition requires on joined rows.
  */
final class HashKeys(
    val leftHash: Product => Int,
    val rightHash: Product => Int,
    val matches: (Product, Product) => Boolean,
    val sameKey: (Product, Product) => Boolean,
    val residual: Option[Product => Boolean]
)

/** Splits and compiles a join condition written with `$left` and `$right`. */
inline def compileJoin[L <: AnyNamedTuple, R <: AnyNamedTuple](inline step: JoinSchemas[L, R] ?=> Column[Boolean])
    : HashKeys = ${ compileJoinImpl[L, R]('step) }

private def compileJoinImpl[L <: AnyNamedTuple: Type, R <: AnyNamedTuple: Type](
    step: Expr[JoinSchemas[L, R] ?=> Column[Boolean]]
)(using Quotes): Expr[HashKeys] =
    import quotes.reflect.*
    val leftArity        = joinArity[L]
    val (keys, residual)   = splitJoin(stepBody(step).asExprOf[Column[Boolean]], leftArity)

    /** Compiles the side of `key` that `side` picks, against rows shifted by `shift`. */
    def compileKey(key: JoinKey[?], side: JoinKey[?] => Expr[Column[?]], shift: Int): Expr[Product => key.tpe.Underlying] =
        import key.tpe
        QuotedColumnCompiler(shift).compile(side(key).asInstanceOf[Expr[Column[key.tpe.Underlying]]])

    /** Combines the hashes of the key columns read by `side`, as compiled against rows shifted by `shift`. */
    def hash(side: JoinKey[?] => Expr[Column[?]], shift: Int): Expr[Product => Int] =
        '{ (row: Product) =>
            ${
                keys.foldLeft(Expr(0)): (acc, key) =>
                    import key.tpe
                    val f = compileKey(key, side, shift)
                    '{ 31 * $acc + ${ Expr.betaReduce('{ $f(row) }) }.## }
            }
        }

    /** Whether every key read by `lhs` on `l` (shifted by `lShift`) equals the one read by `rhs` on `r`. */
    def equal(lhs: JoinKey[?] => Expr[Column[?]], lShift: Int, rhs: JoinKey[?] => Expr[Column[?]], rShift: Int)
        : Expr[(Product, Product) => Boolean] =
        '{ (l: Product, r: Product) =>
            ${
                keys.map: key =>
                    import key.tpe
                    val (fl, fr) = (compileKey(key, lhs, lShift), compileKey(key, rhs, rShift))
                    '{ ${ Expr.betaReduce('{ $fl(l) }) } == ${ Expr.betaReduce('{ $fr(r) }) } }
                .reduceOption((a, b) => '{ $a && $b }).getOrElse(Expr(true))
            }
        }

    val residualFn = residual.reduceOption((l, r) => '{ Column.And($l, $r) }) match
        case Some(c) => '{ Some(${ QuotedColumnCompiler.compile(c) }) }
        case None    => '{ None }
    '{
        HashKeys(
            ${ hash(_.left, 0) },
            ${ hash(_.right, leftArity) },
            ${ equal(_.left, 0, _.right, leftArity) },
            ${ equal(_.right, leftArity, _.right, leftArity) },
            $residualFn
        )
    }

/** The arity of the left side of a join, which must be known statically. */
private def joinArity[L <: AnyNamedTuple: Type](using Quotes): Int =
    import quotes.reflect.*
    Schema.arity[L].getOrElse(report.errorAndAbort(s"Cannot join: the left schema ${Type.show[L]} is not known statically"))

/** An equality between a left and a right expression of type `T`, a hash key of a join. */
private final case class JoinKey[T](left: Expr[Column[T]], right: Expr[Column[T]])(using val tpe: Type[T])

/** Splits a join `condition` into its hash keys, the equalities between an expression reading only left fields and one
  * reading only right ones, and the rest of its conjuncts. Fields at indices below `leftArity` are the left ones.
  */
private def splitJoin(using Quotes)(condition: Expr[Column[Boolean]], leftArity: Int)
    : (List[JoinKey[?]], List[Expr[Column[Boolean]]]) =
    /** Which inputs an expression reads: `Some(true)` only left, `Some(false)` only right, `None` both or neither. */
    def side(col: Expr[Column[?]]): Option[Boolean] =
        refIndices(col).map(_.map(_ < leftArity).distinct) match
            case Some(List(onLeft)) => Some(onLeft)
            case _                  => None
    val parts = splitAnd(condition).map:
        case c @ '{ Column.Equal[t]($a, $b) } =>
            (side(a), side(b)) match
                case (Some(true), Some(false)) => Right(JoinKey(a, b))
                case (Some(false), Some(true)) => Right(JoinKey(b, a))
                case _                         => Left(c)
        case c => Left(c)
    (parts.collect { case Right(key) => key }, parts.collect { case Left(c) => c })

/** The conjuncts of a predicate: `a && (b && c)` gives `a`, `b`, `c`. */
private def splitAnd(using Quotes)(col: Expr[Column[Boolean]]): List[Expr[Column[Boolean]]] =
    stripColumn(col) match
        case '{ Column.And($l, $r) } => splitAnd(l) ++ splitAnd(r)
        case c                       => List(c)

/** Indices of the fields an expression reads, or `None` if some are not known statically. */
private def refIndices(using Quotes)(col: Expr[Column[?]]): Option[List[Int]] =
    stripColumn(col) match
        case '{ type n <: String; Column.Ref[a, n]($name, $index) } => index.value.map(List(_))
        case c =>
            children(c).flatMap(_.foldRight(Option(List.empty[Int]))((child, acc) => refIndices(child).zip(acc).map(_ ++ _)))
