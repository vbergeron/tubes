package tubes

import scala.annotation.implicitNotFound
import NamedTuple.AnyNamedTuple

/** A declarative pipeline spec; `schema` describes the rows it produces. Within each step, `$` is that schema. */
sealed trait Plan[Out <: AnyNamedTuple]:
    def schema: Schema[Out]

object Plan:
    /** Building steps are inline, with an inline receiver, so a plan written in place expands to plan constructors. */
    extension [Out <: AnyNamedTuple](inline plan: Plan[Out])
        inline def filter(inline predicate: Schema[Out] ?=> Column[Boolean]): Plan[Out] =
            FilterSpec(plan, predicate(using plan.schema))

        inline def select[Cols](inline columns: Schema[Out] ?=> Cols)(using p: Projection[Cols]): Plan[p.Out] =
            SelectSpec[p.Out](plan, p.columns(columns(using plan.schema)))

        /** Applies a reusable step to this plan. */
        inline def through[B <: AnyNamedTuple](inline step: Plan[Out] => Plan[B]): Plan[B] = step(plan)

        /** Runs `plan` with the row kernel; the plan must be visible at the call site (see `RowKernel`). */
        inline def materialize(kernel: RowKernel.type): List[Out] = RowKernel.materialize(plan)

    /** Compiles a plan producing `Out` rows into some output `A`, which may depend on `Out` (e.g. `List[Out]`, a query string,
      * a function), compiling column expressions with `columns`.
      */
    trait Compiler[Out <: AnyNamedTuple, F[_]]:
        type A
        def columns: Column.Compiler[F]
        def compile(plan: F[Plan[Out]]): F[A]

    object Compiler:
        type Aux[Out <: AnyNamedTuple, F[_], A0] = Compiler[Out, F] { type A = A0 }

/** Reads rows shaped by `schema`: `Some(source)` holds data to scan, `None` an input left for the compiler to bind. */
final class Scan[Out <: AnyNamedTuple](val source: Option[Source[Out]], val schema: Schema[Out]) extends Plan[Out]:
    override def toString: String = source match
        case None => schema.names.mkString("Scan(?: ", ", ", ")")
        case _    => schema.names.mkString("Scan(", ", ", ")")

/** Keeps the rows of `input` for which `predicate` holds; the row shape is unchanged. */
final class FilterSpec[Out <: AnyNamedTuple](val input: Plan[Out], val predicate: Column[Boolean]) extends Plan[Out]:
    def schema: Schema[Out] = input.schema

    override def toString: String = s"FilterSpec($input, $predicate)"

/** A projection of `input` onto named `columns`; the output schema takes its names from them. */
final class SelectSpec[Out <: AnyNamedTuple](val input: Plan[?], val columns: List[Column[?] & Named[?]]) extends Plan[Out]:
    val schema: Schema[Out] = new Schema[Out](IArray.from(columns.map(_.name)))

    override def toString: String =
        schema.names.zip(columns).map((n, c) => s"$n = $c").mkString(s"SelectSpec($input, ", ", ", ")")

/** Starts a plan reading `source`, whose rows are described by its `Scanable` instance. */
def from[S, R <: AnyNamedTuple](source: S)(
    using @implicitNotFound("Cannot read ${S} as a pipeline source: no Scanable instance") s: Scanable.Aux[S, R]
): Scan[R] =
    val src = Source(source, s)
    Scan(Some(src), src.schema)

/** Starts a plan over an unbound input of `A` rows; the interpreter decides where they come from. */
def from[A <: AnyNamedTuple: Schema]: Scan[A] =
    Scan(None, summon[Schema[A]])
