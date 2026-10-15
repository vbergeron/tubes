package tubes

import scala.quoted.*
import NamedTuple.AnyNamedTuple

/** Row-based kernel: compiles a plan at compile time into one pipeline over in-memory lists of rows.
  *
  * The plan must be visible at the call site: `filter`, `select` and `through` on a `from(...)` scan, with reusable
  * steps declared as `inline def`s taking `inline` plans. Columns are compiled by `QuotedColumnCompiler`.
  */
final class RowKernel[Out <: AnyNamedTuple: Type](using Quotes) extends Plan.Compiler[Out, Expr]:
    import quotes.reflect.*

    type A = List[Out]
    val columns = QuotedColumnCompiler

    def compile(plan: Expr[Plan[Out]]): Expr[List[Out]] =
        '{ ${ rows(plan, Map.empty) }.map(RowKernel.asTuple).asInstanceOf[List[Out]] }

    /** Vals introduced by inlining, by symbol. */
    private type Scope = Map[Symbol, Term]

    private def rows(plan: Expr[Any], scope: Scope): Expr[List[Product]] = peel(plan, scope) match
        case ('{ type o <: AnyNamedTuple; FilterSpec[o]($input, $predicate) }, inner) =>
            val keep = columns.compile(peel(predicate, inner)._1.asExprOf[Column[Boolean]])
            '{ ${ rows(input, inner) }.filter($keep) }
        case ('{ type o <: AnyNamedTuple; SelectSpec[o]($input, $cols) }, inner) =>
            val compiled = projected(cols, inner).map:
                case '{ $c: Column[t] } => columns.compile(c): Expr[Product => Any]
                case other              => report.errorAndAbort(s"RowKernel: not a column: ${other.show}", other)
            '{ ${ rows(input, inner) }.map((row: Product) => ${ Expr.ofTupleFromSeq(compiled.map(f => Expr.betaReduce('{ $f(row) }))) }) }
        case ('{ $scan: Scan[?] }, _) =>
            '{ RowKernel.scan(${ scan.asTerm.changeOwner(Symbol.spliceOwner).asExprOf[Scan[?]] }) }
        case (other, _) =>
            report.errorAndAbort("RowKernel needs the plan written at the call site, with steps as inline defs taking inline plans", other)

    /** The column expressions of a select: the tuple passed to `Projection.columns`. */
    private def projected(cols: Expr[Any], scope: Scope): List[Expr[Any]] = peel(cols, scope) match
        case ('{ ($projection: Projection[c]).columns($tuple) }, inner) =>
            peel(tuple, inner)._1.asTerm match
                case Apply(TypeApply(Select(tupleN, "apply"), _), args) if tupleN.symbol.fullName.startsWith("scala.Tuple") =>
                    args.map(arg => peel(arg.asExpr, inner)._1)
                case other => report.errorAndAbort(s"RowKernel: expected a tuple of columns, got ${other.show}", other.pos)
        case (other, _) => report.errorAndAbort(s"RowKernel: expected the select's columns, got ${other.show}", other)

    /** Looks through inlining wrappers, casts and the vals inlining introduces, recording those vals in the scope. */
    private def peel(expr: Expr[Any], scope: Scope): (Expr[Any], Scope) = expr.asTerm match
        case Inlined(_, bindings, body)                                     => peel(body.asExpr, bind(scope, bindings))
        case Block(stats, body) if stats.forall { case _: ValDef => true; case _ => false } => peel(body.asExpr, bind(scope, stats))
        case Typed(body, _)                                                 => peel(body.asExpr, scope)
        case TypeApply(Select(body, "asInstanceOf" | "$asInstanceOf$"), _)  => peel(body.asExpr, scope)
        case id: Ident if scope.contains(id.symbol)                         => peel(scope(id.symbol).asExpr, scope)
        case _                                                              => (expr, scope)

    private def bind(scope: Scope, stats: List[Statement]): Scope =
        scope ++ stats.collect { case v: ValDef if v.rhs.isDefined => v.symbol -> v.rhs.get }

object RowKernel:
    inline def materialize[Out <: AnyNamedTuple](inline plan: Plan[Out]): List[Out] = ${ materializeImpl('plan) }

    def materializeImpl[Out <: AnyNamedTuple: Type](plan: Expr[Plan[Out]])(using Quotes): Expr[List[Out]] =
        RowKernel[Out]().compile(plan)

    /** Reads the rows of a scan, in its source's own representation. */
    def scan(scan: Scan[?]): List[Product] = scan.source match
        case Some(src) => src.scan.toList
        case None      => sys.error(s"RowKernel has no data bound for $scan")

    /** Output rows must be `Out` tuples; rows still in a source's own representation are converted on the way out. */
    def asTuple(row: Product): Product = row match
        case t: Tuple => t
        case p        => Tuple.fromProduct(p)
