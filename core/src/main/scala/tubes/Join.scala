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

/** A join condition compiled for a hash join: equalities between a left and a right expression become the hash keys,
  * computed by `leftKey` on left rows and `rightKey` on right rows; whatever else the condition requires is checked by
  * `residual` on joined rows.
  */
final class HashKeys(
    val leftKey: Product => Any,
    val rightKey: Product => Any,
    val residual: Option[Product => Boolean]
)

/** Splits and compiles a join condition written with `$left` and `$right`. */
inline def compileJoin[L <: AnyNamedTuple, R <: AnyNamedTuple](inline step: JoinSchemas[L, R] ?=> Column[Boolean])
    : HashKeys = ${ compileJoinImpl[L, R]('step) }

private def compileJoinImpl[L <: AnyNamedTuple: Type, R <: AnyNamedTuple: Type](
    step: Expr[JoinSchemas[L, R] ?=> Column[Boolean]]
)(using Quotes): Expr[HashKeys] =
    import quotes.reflect.*
    val leftArity = Schema.arity[L].getOrElse(report.errorAndAbort(s"Cannot join: the left schema ${Type.show[L]} is not known statically"))
    val conjuncts = splitAnd(stepBody(step).asExprOf[Column[Boolean]])

    /** Which inputs an expression reads: `Some(true)` only left, `Some(false)` only right, `None` both or neither. */
    def side(col: Expr[Column[?]]): Option[Boolean] =
        refIndices(col).map(_.map(_ < leftArity).distinct) match
            case Some(List(onLeft)) => Some(onLeft)
            case _                  => None

    val equalities = conjuncts.map:
        case c @ '{ Column.Equal[t]($a, $b) } =>
            (side(a), side(b)) match
                case (Some(true), Some(false)) => Right((a, b))
                case (Some(false), Some(true)) => Right((b, a))
                case _                         => Left(c)
        case c => Left(c)
    val keys     = equalities.collect { case Right(pair) => pair }
    val residual = equalities.collect { case Left(c) => c }

    def key(cols: List[Expr[Column[?]]], shift: Int): Expr[Product => Any] =
        val compiled = cols.map(c => QuotedColumnCompiler(shift).compile(c.asInstanceOf[Expr[Column[Any]]]))
        compiled match
            case Nil      => '{ (row: Product) => () }
            case f :: Nil => f
            case fs       => '{ (row: Product) => ${ Expr.ofTupleFromSeq(fs.map(f => Expr.betaReduce('{ $f(row) }))) } }

    val residualFn = residual.reduceOption((l, r) => '{ Column.And($l, $r) }) match
        case Some(c) => '{ Some(${ QuotedColumnCompiler.compile(c) }) }
        case None    => '{ None }
    '{ HashKeys(${ key(keys.map(_._1), 0) }, ${ key(keys.map(_._2), leftArity) }, $residualFn) }

/** The conjuncts of a predicate: `a && (b && c)` gives `a`, `b`, `c`. */
private def splitAnd(using Quotes)(col: Expr[Column[Boolean]]): List[Expr[Column[Boolean]]] =
    stripColumn(col) match
        case '{ Column.And($l, $r) } => splitAnd(l) ++ splitAnd(r)
        case c                       => List(c)

/** Indices of the fields an expression reads, or `None` if some are not known statically. */
private def refIndices(using Quotes)(col: Expr[Column[?]]): Option[List[Int]] =
    def both(l: Expr[Column[?]], r: Expr[Column[?]]) = refIndices(l).zip(refIndices(r)).map(_ ++ _)
    stripColumn(col) match
        case '{ type n <: String; Column.Ref[a, n]($name, $index) } => index.value.map(List(_))
        case '{ type n <: String; Column.Alias[a, n]($c, $name) }   => refIndices(c)
        case '{ Column.Lit[a]($value) }                             => Some(Nil)
        case '{ Column.Arith[a]($op, $l, $r, $num) }                => both(l, r)
        case '{ Column.Compare[a]($op, $l, $r, $ord) }              => both(l, r)
        case '{ Column.Equal[a]($l, $r) }                           => both(l, r)
        case '{ Column.And($l, $r) }                                => both(l, r)
        case '{ Column.Or($l, $r) }                                 => both(l, r)
        case '{ Column.Not($c) }                                    => refIndices(c)
        case _                                                      => None
