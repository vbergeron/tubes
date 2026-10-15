package tubes

import scala.annotation.implicitNotFound
import NamedTuple.{AnyNamedTuple, DropNames, Names}

/** A declarative pipeline spec; `schema` describes the rows it produces. Within each step, `$` is that schema. */
sealed trait Plan[Out <: AnyNamedTuple]:
    def schema: Schema[Out]

object Plan:
    /** `filter` and `select` are inline so the column expression written at their call site is compiled right there;
      * nodes keep both the `Column` AST and its compiled row function.
      */
    extension [Out <: AnyNamedTuple](plan: Plan[Out])
        inline def filter(inline predicate: Schema[Out] ?=> Column[Boolean]): Plan[Out] =
            FilterSpec(plan, predicate(using plan.schema), compileStep(predicate))

        inline def select[Cols](inline columns: Schema[Out] ?=> Cols)(using p: Projection[Cols]): Plan[p.Out] =
            SelectSpec[p.Out](plan, p.columns(columns(using plan.schema)), compileProjection(columns))

        /** Pairs every row with the `right` rows for which `condition` holds, written with `$left` and `$right`. The
          * joined rows have this plan's fields followed by the right ones, so the field names must not clash: rename
          * them with a `select` beforehand if they do.
          */
        inline def join[R <: AnyNamedTuple](right: Plan[R])(inline condition: JoinSchemas[Out, R] ?=> Column[Boolean])(
            using @implicitNotFound("Cannot join ${Out} with ${R}: their field names overlap, rename them with select first")
            disjoint: Tuple.Disjoint[Names[Out], Names[R]] =:= true
        ): Plan[NamedTuple.Concat[Out, R]] = joinSpec(right, JoinKind.Inner)(condition)

        /** Like `join`, and also keeps the rows matching no right row: the right fields are optional, `None` there. */
        inline def leftJoin[R <: AnyNamedTuple](right: Plan[R])(
            inline condition: JoinSchemas[Out, R] ?=> Column[Boolean]
        )(
            using @implicitNotFound("Cannot join ${Out} with ${R}: their field names overlap, rename them with select first")
            disjoint: Tuple.Disjoint[Names[Out], Names[R]] =:= true
        ): Plan[NamedTuple.Concat[Out, Optional[R]]] = joinSpec(right, JoinKind.Left)(condition)

        /** Like `join`, and also keeps the right rows matching no row: this plan's fields are optional, `None` there.
          * The unmatched right rows come last.
          */
        inline def rightJoin[R <: AnyNamedTuple](right: Plan[R])(
            inline condition: JoinSchemas[Out, R] ?=> Column[Boolean]
        )(
            using @implicitNotFound("Cannot join ${Out} with ${R}: their field names overlap, rename them with select first")
            disjoint: Tuple.Disjoint[Names[Out], Names[R]] =:= true
        ): Plan[NamedTuple.Concat[Optional[Out], R]] = joinSpec(right, JoinKind.Right)(condition)

        /** Like `join`, and also keeps the unmatched rows of both sides: every field is optional, `None` on the
          * missing side. The unmatched right rows come last.
          */
        inline def fullJoin[R <: AnyNamedTuple](right: Plan[R])(
            inline condition: JoinSchemas[Out, R] ?=> Column[Boolean]
        )(
            using @implicitNotFound("Cannot join ${Out} with ${R}: their field names overlap, rename them with select first")
            disjoint: Tuple.Disjoint[Names[Out], Names[R]] =:= true
        ): Plan[NamedTuple.Concat[Optional[Out], Optional[R]]] = joinSpec(right, JoinKind.Full)(condition)

        /** The condition is written against both inputs as they are, whatever optional fields the join outputs. */
        private inline def joinSpec[R <: AnyNamedTuple, J <: AnyNamedTuple](right: Plan[R], kind: JoinKind)(
            inline condition: JoinSchemas[Out, R] ?=> Column[Boolean]
        ): Plan[J] =
            val schemas = JoinSchemas(plan.schema, RightSchema[Out, R](right.schema, plan.schema.names.length))
            JoinSpec[J](
                plan,
                right,
                kind,
                condition(using schemas),
                compileJoin[Out, R](condition),
                optionFields[DropNames[Out]],
                optionFields[DropNames[R]]
            )

        /** Applies a reusable step to this plan. */
        def through[B <: AnyNamedTuple](step: Plan[Out] => Plan[B]): Plan[B] = step(plan)

        def materialize(compiler: Plan.Compiler[Out]): compiler.A = compiler.compile(plan)

    /** Compiles a plan producing `Out` rows into some output `A`, which may depend on `Out` (e.g. `List[Out]`, a query string,
      * a function).
      */
    trait Compiler[Out <: AnyNamedTuple]:
        type A
        def compile(plan: Plan[Out]): A

    object Compiler:
        type Aux[Out <: AnyNamedTuple, A0] = Compiler[Out] { type A = A0 }

/** Reads rows shaped by `schema`: `Some(source)` holds data to scan, `None` an input left for the compiler to bind. */
final class Scan[Out <: AnyNamedTuple](val source: Option[Source[Out]], val schema: Schema[Out]) extends Plan[Out]:
    override def toString: String = source match
        case None => schema.names.mkString("Scan(?: ", ", ", ")")
        case _    => schema.names.mkString("Scan(", ", ", ")")

/** Keeps the rows of `input` for which `predicate` holds; the row shape is unchanged. `compiled` evaluates `predicate`. */
final class FilterSpec[Out <: AnyNamedTuple](
    val input: Plan[Out],
    val predicate: Column[Boolean],
    val compiled: Product => Boolean
) extends Plan[Out]:
    def schema: Schema[Out] = input.schema

    override def toString: String = s"FilterSpec($input, $predicate)"

/** A projection of `input` onto named `columns`; the output schema takes its names from them. `compiled` builds an
  * output row from an input row.
  */
final class SelectSpec[Out <: AnyNamedTuple](
    val input: Plan[?],
    val columns: List[Column[?] & Named[?]],
    val compiled: Product => Product
) extends Plan[Out]:
    val schema: Schema[Out] = new Schema[Out](IArray.from(columns.map(_.name)))

    override def toString: String =
        schema.names.zip(columns).map((n, c) => s"$n = $c").mkString(s"SelectSpec($input, ", ", ", ")")

/** `A` made optional: `Option[A]`, or `A` itself if it already is one. There is a single level of absence, as SQL's
  * `NULL`, so a missing optional value is `None` too.
  */
type Nullable[A] = A match
    case Option[x] => Option[x]
    case _         => Option[A]

/** The fields of `NT`, each made optional. */
type Optional[NT <: AnyNamedTuple] = NamedTuple.NamedTuple[Names[NT], Tuple.Map[DropNames[NT], Nullable]]

/** Which of the fields `T` are already optional, so an outer join does not wrap them again. */
private[tubes] inline def optionFields[T <: Tuple]: IArray[Boolean] = IArray.from(optionFieldList[T])

private inline def optionFieldList[T <: Tuple]: List[Boolean] = inline compiletime.erasedValue[T] match
    case _: EmptyTuple          => Nil
    case _: (Option[?] *: rest) => true :: optionFieldList[rest]
    case _: (? *: rest)         => false :: optionFieldList[rest]

/** Which unmatched rows a join keeps besides the matching pairs: none, the left ones, the right ones or both. */
enum JoinKind(val keepsLeft: Boolean, val keepsRight: Boolean):
    case Inner extends JoinKind(false, false)
    case Left  extends JoinKind(true, false)
    case Right extends JoinKind(false, true)
    case Full  extends JoinKind(true, true)

/** Pairs the rows of `left` and `right` for which `condition` holds, plus the unmatched rows `kind` keeps; the output
  * rows hold the left fields followed by the right ones, optional on the sides an outer join may leave missing.
  * `keys` compiles `condition` for a hash join. `leftOptions` and `rightOptions` tell which input fields are already
  * optional, so they stay as they are on a side an outer join makes optional.
  */
final class JoinSpec[Out <: AnyNamedTuple](
    val left: Plan[?],
    val right: Plan[?],
    val kind: JoinKind,
    val condition: Column[Boolean],
    val keys: HashKeys,
    val leftOptions: IArray[Boolean],
    val rightOptions: IArray[Boolean]
) extends Plan[Out]:
    val schema: Schema[Out] = new Schema(left.schema.names ++ right.schema.names)

    override def toString: String = s"JoinSpec($kind, $left, $right, $condition)"

/** Starts a plan reading `source`, whose rows are described by its `Scanable` instance. */
def from[S, R <: AnyNamedTuple](source: S)(
    using @implicitNotFound("Cannot read ${S} as a pipeline source: no Scanable instance") s: Scanable.Aux[S, R]
): Scan[R] =
    val src = Source(source, s)
    Scan(Some(src), src.schema)

/** Starts a plan over an unbound input of `A` rows; the interpreter decides where they come from. */
def from[A <: AnyNamedTuple: Schema]: Scan[A] =
    Scan(None, summon[Schema[A]])
