package tubes

import scala.annotation.implicitNotFound
import compiletime.constValueTuple
import NamedTuple.{AnyNamedTuple, NamedTuple}

/** How a tuple of column expressions `C` becomes named output columns typed by `Out`. */
@implicitNotFound("Cannot select ${C}: every column must be named, with .as(\"name\") or a named tuple")
trait Projection[C]:
    type Out <: AnyNamedTuple
    def columns(c: C): List[Column[?] & Named[?]]

object Projection:
    type Aux[C, O <: AnyNamedTuple] = Projection[C] { type Out = O }

    /** Value types of a tuple of columns. */
    type Values[V <: Tuple] <: Tuple = V match
        case EmptyTuple    => EmptyTuple
        case Column[a] *: t => a *: Values[t]

    private def make[C, O <: AnyNamedTuple](f: C => List[Column[?] & Named[?]]): Aux[C, O] =
        new Projection[C]:
            type Out = O
            def columns(c: C) = f(c)

    /** `(a = $.x, b = $.y + 1)` aliases each column to its field name. */
    inline given named[N <: Tuple, V <: Tuple](using Tuple.Union[V] <:< Column[?])
        : Aux[NamedTuple[N, V], NamedTuple[N, Values[V]]] =
        val labels = constValueTuple[N].toList.map(_.toString)
        make: c =>
            labels.zip(c.toTuple.toList.asInstanceOf[List[Column[?]]]).map:
                case (label, col: Named[?]) if col.name == label => col.asInstanceOf[Column[?] & Named[?]]
                case (label, col)                                => Column.Alias(col, label)

    /** `($.x, ($.y + 1).as("z"))` takes each output name from the column itself. */
    given empty: Aux[EmptyTuple, NamedTuple[EmptyTuple, EmptyTuple]] = make(_ => Nil)

    given cons[N <: String, A, T <: Tuple, ON <: Tuple, OV <: Tuple](using t: Aux[T, NamedTuple[ON, OV]])
        : Aux[(Column[A] & Named[N]) *: T, NamedTuple[N *: ON, A *: OV]] =
        make(c => c.head :: t.columns(c.tail))
