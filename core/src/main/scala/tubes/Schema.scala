package tubes

import scala.quoted.*
import compiletime.constValueTuple
import NamedTuple.{AnyNamedTuple, DropNames, From, Names}

/** Field access `schema.foo` yields a `Column[A] & Named["foo"]` reference typed after the schema. */
final class Schema[NT <: AnyNamedTuple](val names: IArray[String]) extends Selectable:
    type Fields = NamedTuple.NamedTuple[Names[NT], Column.NamedFields[Names[NT], DropNames[NT]]]

    private val indices: Map[String, Int] = names.zipWithIndex.toMap

    /** Resolved at compile time to `Column.Ref[A, "foo"]("foo", index)` when `NT` is known. */
    transparent inline def selectDynamic(inline name: String): Column[?] = ${ Schema.refImpl[NT]('this, 'name) }

    /** Runtime lookup, for schemas whose type is not known statically. */
    def ref(name: String): Column[?] = Column.Ref[Any, String](name, indices(name))

object Schema:
    inline def of[NT <: AnyNamedTuple]: Schema[NT] =
        new Schema(IArray.from(constValueTuple[Names[NT]].toList.map(_.toString)))

    /** Derives a schema from a case class, with one column per field in declaration order. */
    inline def derived[C <: Product]: Schema[From[C]] = of[From[C]]

    /** Every named tuple has its own schema, so `A: Schema` holds for any named tuple `A`. */
    inline given namedTuple[NT <: AnyNamedTuple]: Schema[NT] = of[NT]

    def refImpl[NT <: AnyNamedTuple: Type](self: Expr[Schema[NT]], name: Expr[String])(using Quotes): Expr[Column[?]] =
        staticRef[NT](name, 0).getOrElse('{ $self.ref($name) })

    /** `Column.Ref[A, "foo"]("foo", offset + index)` when `NT` and `name` are known statically. */
    private[tubes] def staticRef[NT <: AnyNamedTuple: Type](name: Expr[String], offset: Int)(using Quotes)
        : Option[Expr[Column[?]]] =
        import quotes.reflect.*
        (tupleElements(TypeRepr.of[Names[NT]]), tupleElements(TypeRepr.of[DropNames[NT]]), name.value) match
            case (Some(names), Some(types), Some(n)) =>
                val index = names.indexWhere(_ =:= ConstantType(StringConstant(n)))
                (types(index).asType, ConstantType(StringConstant(n)).asType) match
                    case ('[a], '[type label <: String; label]) =>
                        Some('{ Column.Ref[a, label]($name, ${ Expr(offset + index) }) })
                    case _ => None
            case _ => None

    /** Number of fields of `NT`, when statically known. */
    private[tubes] def arity[NT <: AnyNamedTuple: Type](using Quotes): Option[Int] =
        import quotes.reflect.*
        tupleElements(TypeRepr.of[Names[NT]]).map(_.size)

    /** Element types of a tuple type written either as `TupleN[..]` or as a `*:` chain. */
    private[tubes] def tupleElements(using Quotes)(tpe: quotes.reflect.TypeRepr): Option[List[quotes.reflect.TypeRepr]] =
        import quotes.reflect.*
        tpe.simplified.dealias match
            case AppliedType(tycon, List(head, tail)) if tycon.typeSymbol == TypeRepr.of[*:].typeSymbol =>
                tupleElements(tail).map(head :: _)
            case t if t =:= TypeRepr.of[EmptyTuple] => Some(Nil)
            case AppliedType(tycon, args) if args.nonEmpty && tycon.typeSymbol == defn.TupleClass(args.size) => Some(args)
            case _ => None
