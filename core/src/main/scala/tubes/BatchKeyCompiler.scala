package tubes

import scala.quoted.*
import BatchColumnCompiler.{Repr, bind, element, repr}

/** The key columns of a `groupBy`, compiled for the batch kernel: each column's function where it could be compiled,
  * and the loops of a key map for their types, unless the kernel's own maps suit them better.
  */
final class BatchKeys(val columns: List[Option[BatchColumn]], val codec: Option[KeyCodec])

object BatchKeys:
    /** Nothing compiled: the kernel interprets the key columns and picks a key map from their arrays. */
    val none: BatchKeys = BatchKeys(Nil, None)

/** Compiles the key columns of a `groupBy` lambda for the batch kernel. */
inline def compileBatchKeys[S, K](inline step: S ?=> K): BatchKeys = ${ compileBatchKeysImpl('step) }

private def compileBatchKeysImpl[S: Type, K: Type](step: Expr[S ?=> K])(using Quotes): Expr[BatchKeys] =
    import quotes.reflect.*
    val columns = tupleElements(stepBody(step).asTerm).map:
        case '{ $c: Column[t] } => (BatchColumnCompiler.compile(c), Type.of[t])
        case other              => report.errorAndAbort(s"Not a column: ${other.show}", other)
    '{ BatchKeys(${ Expr.ofList(columns.map(_._1)) }, ${ BatchKeyCompiler.codec(columns.map(_._2)) }) }

/** Generates the loops of key maps for keys of known types. */
private object BatchKeyCompiler:

    /** How a key column's values are held and compared: as their primitive type where they have one, as `String`s, or
      * as any other boxed values.
      */
    private enum Kind:
        case Primitive(repr: Repr)
        case Text
        case Other

    /** The loops of a key map for key columns of `types`; `None` for a single `Int` or `Long` column, which the kernel
      * numbers by direct addressing or in a table of primitives.
      */
    def codec(types: List[Type[?]])(using Quotes): Expr[Option[KeyCodec]] =
        import quotes.reflect.*
        val kinds = types.map:
            case '[t] =>
                repr[t] match
                    case Repr.Boxed if TypeRepr.of[t].dealias =:= TypeRepr.of[String] => Kind.Text
                    case Repr.Boxed                                                    => Kind.Other
                    case r                                                             => Kind.Primitive(r)
        kinds match
            case List(Kind.Primitive(Repr.IntR | Repr.LongR)) => '{ None }
            case _                                            => '{ Some(${ generate(kinds) }) }

    private def reprOf(kind: Kind): Repr = kind match
        case Kind.Primitive(r) => r
        case _                 => Repr.Boxed

    private def generate(kinds: List[Kind])(using Quotes): Expr[KeyCodec] =
        val reprs = kinds.map(reprOf)
        val arrays = Varargs(reprs.map:
            case Repr.IntR     => '{ new Array[Int](16) }
            case Repr.LongR    => '{ new Array[Long](16) }
            case Repr.DoubleR  => '{ new Array[Double](16) }
            case Repr.BooleanR => '{ new Array[Boolean](16) }
            case Repr.Boxed    => '{ new Array[AnyRef](16) })
        '{
            KeyCodec(
                () => Array[AnyRef]($arrays*).asInstanceOf[Array[Array[?]]],
                (t: KeyTable, keys: IndexedSeq[Array[?]], n: Int) => ${ number(kinds, 't, 'keys, 'n, insert = true) },
                (t: KeyTable, keys: IndexedSeq[Array[?]], n: Int) => ${ number(kinds, 't, 'keys, 'n, insert = false) }
            )
        }

    /** The ids of the keys of the first `n` rows of the `keys` columns in table `t`, inserting the new ones when `insert`
      * and giving them `-1` otherwise. As in `OpenKeyMap`, keys are hashed by combining their values' `##` and compared
      * with `==`, but on values read from arrays of their types, and `String`s through their own methods.
      */
    private def number(kinds: List[Kind], t: Expr[KeyTable], keys: Expr[IndexedSeq[Array[?]]], n: Expr[Int], insert: Boolean)(
        using Quotes
    ): Expr[Array[Int]] =
        val reprs   = kinds.map(reprOf)
        val columns = reprs.zipWithIndex.map((r, c) => (c, r, BatchColumnCompiler.normalize('{ $keys(${ Expr(c) }) }, r)))
        bind(columns): bound =>
            val values = bound.sortBy(_._1).map(_._2).zip(reprs)
            /** The value of key column `c` at row `i`. */
            def value(c: Int, i: Expr[Int]): Expr[Any] = values(c) match
                case (a, r) => element[Any](a, r, i)
            /** The values of key column `c` kept in the table. */
            def kept(c: Int): Expr[Array[?]] = '{ $t.stored(${ Expr(c) }) }
            def hash(i: Expr[Int]): Expr[Int] =
                reprs.indices.foldLeft(Expr(0))((h, c) => '{ 31 * $h + ${ hashOf(value(c, i), kinds(c)) } })
            def same(id: Expr[Int], i: Expr[Int]): Expr[Boolean] =
                reprs.indices
                    .map(c => equal(element[Any](retyped(kept(c), reprs(c)), reprs(c), id), value(c, i), kinds(c)))
                    .reduceOption((a, b) => '{ $a && $b })
                    .getOrElse(Expr(true))
            def store(id: Expr[Int], i: Expr[Int]): Expr[Unit] =
                Expr.block(reprs.indices.toList.map(c => assign(retyped(kept(c), reprs(c)), reprs(c), id, value(c, i))), '{ () })
            '{
                val ids = new Array[Int]($n)
                var i   = 0
                while i < $n do
                    val row  = i
                    val h    = KeyTable.mix(${ hash('row) }.toLong)
                    var slot = h & $t.mask
                    var id   = $t.slots(slot)
                    while id >= 0 && !($t.hashes(id) == h && ${ same('id, 'row) }) do
                        slot = (slot + 1) & $t.mask
                        id = $t.slots(slot)
                    ${
                        if insert then
                            '{
                                if id < 0 then
                                    id = $t.add(h)
                                    ${ store('id, 'row) }
                                    $t.place(slot, id)
                            }
                        else '{ () }
                    }
                    ids(row) = id
                    i += 1
                ids
            }

    /** `values` cast to the array type `repr` holds values in. */
    private def retyped(values: Expr[Array[?]], repr: Repr)(using Quotes): Expr[Array[?]] =
        BatchColumnCompiler.arrayType(repr) match
            case '[t] => '{ $values.asInstanceOf[t] }.asExprOf[Array[?]]

    /** The `##` of `value`, computed on the primitive where it is one; a `String`'s is its `hashCode`, 0 for `null`. */
    private def hashOf(value: Expr[Any], kind: Kind)(using Quotes): Expr[Int] = kind match
        case Kind.Primitive(Repr.IntR)     => '{ ${ value.asExprOf[Int] }.## }
        case Kind.Primitive(Repr.LongR)    => '{ ${ value.asExprOf[Long] }.## }
        case Kind.Primitive(Repr.DoubleR)  => '{ ${ value.asExprOf[Double] }.## }
        case Kind.Primitive(Repr.BooleanR) => '{ ${ value.asExprOf[Boolean] }.## }
        case Kind.Text                     => '{ java.util.Objects.hashCode($value) }
        case _                             => '{ $value.## }

    /** Whether `a == b`, compared as primitives where they are, and `String`s with `equals`, nulls included. */
    private def equal(a: Expr[Any], b: Expr[Any], kind: Kind)(using Quotes): Expr[Boolean] = kind match
        case Kind.Primitive(Repr.IntR)     => '{ ${ a.asExprOf[Int] } == ${ b.asExprOf[Int] } }
        case Kind.Primitive(Repr.LongR)    => '{ ${ a.asExprOf[Long] } == ${ b.asExprOf[Long] } }
        case Kind.Primitive(Repr.DoubleR)  => '{ ${ a.asExprOf[Double] } == ${ b.asExprOf[Double] } }
        case Kind.Primitive(Repr.BooleanR) => '{ ${ a.asExprOf[Boolean] } == ${ b.asExprOf[Boolean] } }
        case Kind.Text                     => '{ java.util.Objects.equals($a, $b) }
        case _                             => '{ $a == $b }

    /** Sets element `i` of `values`, held as `repr`, to `value`. */
    private def assign(values: Expr[Array[?]], repr: Repr, i: Expr[Int], value: Expr[Any])(using Quotes): Expr[Unit] =
        repr match
            case Repr.IntR     => '{ ${ values.asExprOf[Array[Int]] }($i) = ${ value.asExprOf[Int] } }
            case Repr.LongR    => '{ ${ values.asExprOf[Array[Long]] }($i) = ${ value.asExprOf[Long] } }
            case Repr.DoubleR  => '{ ${ values.asExprOf[Array[Double]] }($i) = ${ value.asExprOf[Double] } }
            case Repr.BooleanR => '{ ${ values.asExprOf[Array[Boolean]] }($i) = ${ value.asExprOf[Boolean] } }
            case Repr.Boxed    => '{ ${ values.asExprOf[Array[AnyRef]] }($i) = $value.asInstanceOf[AnyRef] }
