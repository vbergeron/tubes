package tubes

import scala.annotation.implicitNotFound
import NamedTuple.{AnyNamedTuple, NamedTuple}

/** How a tuple of column expressions `C` becomes named output columns typed by `Out`. */
@implicitNotFound("Cannot select ${C}: every column must be named, with .as(\"name\")")
trait Projection[C]:
    type Out <: AnyNamedTuple
    def columns(c: C): List[Column[?] & Named[?]]

object Projection:
    type Aux[C, O <: AnyNamedTuple] = Projection[C] { type Out = O }

    private def make[C, O <: AnyNamedTuple](f: C => List[Column[?] & Named[?]]): Aux[C, O] =
        new Projection[C]:
            type Out = O
            def columns(c: C) = f(c)

    /** `($.x, ($.y + 1).as("z"))` takes each output name from the column itself. */
    given empty: Aux[EmptyTuple, NamedTuple[EmptyTuple, EmptyTuple]] = make(_ => Nil)

    given cons[N <: String, A, T <: Tuple, ON <: Tuple, OV <: Tuple](using t: Aux[T, NamedTuple[ON, OV]])
        : Aux[(Column[A] & Named[N]) *: T, NamedTuple[N *: ON, A *: OV]] =
        make(c => c.head :: t.columns(c.tail))
