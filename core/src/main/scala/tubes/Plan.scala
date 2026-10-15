package tubes

import scala.annotation.implicitNotFound
import NamedTuple.{AnyNamedTuple, Names}

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

        /** Starts a join with `right`; `.on(condition)` completes it. */
        def join[R <: AnyNamedTuple](right: Plan[R]): JoinBuilder[Out, R] = JoinBuilder(plan, right)

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

/** A join waiting for its condition. */
final class JoinBuilder[L <: AnyNamedTuple, R <: AnyNamedTuple](val left: Plan[L], val right: Plan[R])

object JoinBuilder:
    extension [L <: AnyNamedTuple, R <: AnyNamedTuple](builder: JoinBuilder[L, R])
        /** Pairs every left row with the right rows for which `condition` holds, written with `$left` and `$right`.
          * The joined rows have the left fields followed by the right ones, so the field names must not clash: rename
          * them with a `select` beforehand if they do.
          */
        inline def on(inline condition: JoinSchemas[L, R] ?=> Column[Boolean])(
            using @implicitNotFound("Cannot join ${L} with ${R}: their field names overlap, rename them with select first")
            disjoint: Tuple.Disjoint[Names[L], Names[R]] =:= true
        ): Plan[NamedTuple.Concat[L, R]] =
            val (left, right) = (builder.left, builder.right)
            val schemas = JoinSchemas(left.schema, RightSchema[L, R](right.schema, left.schema.names.length))
            JoinSpec[L, R](left, right, condition(using schemas), compileJoin[L, R](condition))

/** Pairs the rows of `left` and `right` for which `condition` holds; the output rows hold the left fields followed by
  * the right ones. `keys` compiles `condition` for a hash join.
  */
final class JoinSpec[L <: AnyNamedTuple, R <: AnyNamedTuple](
    val left: Plan[L],
    val right: Plan[R],
    val condition: Column[Boolean],
    val keys: HashKeys
) extends Plan[NamedTuple.Concat[L, R]]:
    val schema: Schema[NamedTuple.Concat[L, R]] = new Schema(left.schema.names ++ right.schema.names)

    override def toString: String = s"JoinSpec($left, $right, $condition)"

/** Starts a plan reading `source`, whose rows are described by its `Scanable` instance. */
def from[S, R <: AnyNamedTuple](source: S)(
    using @implicitNotFound("Cannot read ${S} as a pipeline source: no Scanable instance") s: Scanable.Aux[S, R]
): Scan[R] =
    val src = Source(source, s)
    Scan(Some(src), src.schema)

/** Starts a plan over an unbound input of `A` rows; the interpreter decides where they come from. */
def from[A <: AnyNamedTuple: Schema]: Scan[A] =
    Scan(None, summon[Schema[A]])
