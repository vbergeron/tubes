package tubes

import scala.quoted.*

/** Compiles quoted column trees, written with `Column` constructors, into quoted row functions. */
def QuotedColumnCompiler(using Quotes): Column.Compiler[Expr] = QuotedColumnCompiler(0)

/** Same as `QuotedColumnCompiler`, for rows holding field `index` at position `index - shift`: compiles the right-hand
  * expressions of a join, whose references point into the joined row, to read right rows directly.
  */
def QuotedColumnCompiler(shift: Int)(using Quotes): Column.Compiler[Expr] = new Column.Compiler[Expr]:
    def compile[A](col: Expr[Column[A]]): Expr[Product => A] = (col: Expr[Any]) match
        case '{ $c: Column[t] } => typed[t](c).asInstanceOf[Expr[Product => A]]

    /** Same as `compile`, with the column's value type available to the quotes. */
    private def typed[A: Type](expr: Expr[Column[A]]): Expr[Product => A] = stripColumn(expr) match
        case '{ type n <: String; Column.Ref[A, n]($name, $index) } =>
            val at = index.value match
                case Some(i) => Expr(i - shift)
                case None    => if shift == 0 then index else '{ $index - ${ Expr(shift) } }
            '{ (row: Product) => row.productElement($at).asInstanceOf[A] }
        case '{ type n <: String; Column.Alias[A, n]($c, $name) }   => typed(c)
        case '{ Column.Lit[A]($value) }              => '{ (row: Product) => $value }
        case '{ Column.Arith[A]($op, $l, $r, $num) } =>
            val (fl, fr) = (typed(l), typed(r))
            op match
                case '{ ArithOp.Plus }  => '{ (row: Product) => $num.plus($fl(row), $fr(row)) }
                case '{ ArithOp.Minus } => '{ (row: Product) => $num.minus($fl(row), $fr(row)) }
                case '{ ArithOp.Times } => '{ (row: Product) => $num.times($fl(row), $fr(row)) }
        case '{ Column.Compare[t]($op, $l, $r, $ord) } =>
            val (fl, fr) = (typed(l), typed(r))
            val f = op match
                case '{ CmpOp.Lt } => '{ (row: Product) => $ord.lt($fl(row), $fr(row)) }
                case '{ CmpOp.Le } => '{ (row: Product) => $ord.lteq($fl(row), $fr(row)) }
                case '{ CmpOp.Gt } => '{ (row: Product) => $ord.gt($fl(row), $fr(row)) }
                case '{ CmpOp.Ge } => '{ (row: Product) => $ord.gteq($fl(row), $fr(row)) }
            f.asExprOf[Product => A]
        case '{ Column.Equal[t]($l, $r) }            =>
            val (fl, fr) = (typed(l), typed(r))
            '{ (row: Product) => $fl(row) == $fr(row) }.asExprOf[Product => A]
        case '{ Column.And($l, $r) }                 =>
            val (fl, fr) = (typed(l), typed(r))
            '{ (row: Product) => $fl(row) && $fr(row) }.asExprOf[Product => A]
        case '{ Column.Or($l, $r) }                  =>
            val (fl, fr) = (typed(l), typed(r))
            '{ (row: Product) => $fl(row) || $fr(row) }.asExprOf[Product => A]
        case '{ Column.Not($c) }                     =>
            val f = typed(c)
            '{ (row: Product) => !$f(row) }.asExprOf[Product => A]
        case '{ Column.Default[A]($c, $value) }      =>
            val (fc, fv) = (typed(c), typed(value))
            '{ (row: Product) => $fc(row) match
                case Some(a) => a
                case None    => $fv(row)
            }
        case _ => quotes.reflect.report.errorAndAbort(s"Cannot compile column expression ${expr.show}", expr)

/** Drops what inlining, `Selectable` and named tuples leave around an expression: ascriptions, casts, and inlined
  * wrappers or blocks binding local vals, whose result it follows through those vals. Aborts if what remains still
  * reads one of them.
  */
private[tubes] def peel(using Quotes)(term: quotes.reflect.Term): quotes.reflect.Term =
    import quotes.reflect.*
    def isVal(stat: Statement) = stat match
        case ValDef(_, _, Some(_)) => true
        case _                     => false
    def bind(stats: List[Statement], locals: Map[Symbol, Term]) =
        locals ++ stats.collect { case v @ ValDef(_, _, Some(rhs)) => v.symbol -> rhs }
    def loop(t: Term, locals: Map[Symbol, Term]): (Term, Map[Symbol, Term]) = t match
        case Inlined(_, stats, body) if stats.forall(isVal)                => loop(body, bind(stats, locals))
        case Block(stats, body) if stats.forall(isVal)                     => loop(body, bind(stats, locals))
        case Typed(body, _)                                                => loop(body, locals)
        case TypeApply(Select(body, "asInstanceOf" | "$asInstanceOf$"), _) => loop(body, locals)
        case id: Ident if locals.contains(id.symbol)                       => loop(locals(id.symbol), locals)
        case _                                                             => (t, locals)
    val (peeled, locals) = loop(term, Map.empty)
    val readsLocal = new TreeAccumulator[Boolean]:
        def foldTree(found: Boolean, tree: Tree)(owner: Symbol): Boolean = found || (tree match
            case id: Ident => locals.contains(id.symbol)
            case _         => foldOverTree(false, tree)(owner))
    if readsLocal.foldTree(false, peeled)(Symbol.spliceOwner) then
        report.errorAndAbort(s"Cannot read ${peeled.show}: it depends on local definitions", term.pos)
    peeled

/** Same as `peel`, for column expressions. */
private[tubes] def stripColumn[C <: Column[?]](using Quotes)(expr: Expr[C]): Expr[C] =
    import quotes.reflect.*
    peel(expr.asTerm).asExpr.asInstanceOf[Expr[C]]

/** The columns a `Column` constructor call is built from, such as both sides of an `Arith`; `None` if `col` is not
  * such a call. Analyses of column trees recurse through this rather than listing every constructor.
  */
private[tubes] def children(using Quotes)(col: Expr[Column[?]]): Option[List[Expr[Column[?]]]] =
    import quotes.reflect.*
    val columnCases = Symbol.requiredModule("tubes.Column").moduleClass
    /** The declared parameter types of the case constructor `fun` calls, before its type arguments are applied. */
    def constructorParams(fun: Term): Option[List[TypeRepr]] = fun match
        case TypeApply(f, _) => constructorParams(f)
        case _ if fun.symbol.name == "apply" && fun.symbol.owner.companionClass.maybeOwner == columnCases =>
            fun.tpe.widen match
                case PolyType(_, _, MethodType(_, params, _)) => Some(params)
                case MethodType(_, params, _)                 => Some(params)
                case _                                        => None
        case _ => None
    stripColumn(col).asTerm match
        case Apply(fun, args) =>
            constructorParams(fun).map: params =>
                params.zip(args).collect:
                    case (param, arg) if param <:< TypeRepr.of[Column[?]] => arg.asExprOf[Column[?]]
        case _ => None

/** Compiles `col` into a row function at compile time; it must be written with `Column` constructors. */
inline def compileColumn[A](inline col: Column[A]): Product => A = ${ compileColumnImpl('col) }

private def compileColumnImpl[A: Type](col: Expr[Column[A]])(using Quotes): Expr[Product => A] =
    QuotedColumnCompiler.compile(col)

/** Compiles the column computed by a step lambda, such as a `filter` predicate written with `$`. */
inline def compileStep[S, A](inline step: S ?=> Column[A]): Product => A = ${ compileStepImpl('step) }

/** Compiles the tuple of columns computed by a `select` lambda into one function building the output row. */
inline def compileProjection[S, C](inline step: S ?=> C): Product => Product = ${ compileProjectionImpl('step) }

private def compileStepImpl[S: Type, A: Type](step: Expr[S ?=> Column[A]])(using Quotes): Expr[Product => A] =
    QuotedColumnCompiler.compile(stepBody(step).asExprOf[Column[A]])

private def compileProjectionImpl[S: Type, C: Type](step: Expr[S ?=> C])(using Quotes): Expr[Product => Product] =
    import quotes.reflect.*
    val compiled = tupleElements(stepBody(step).asTerm).map:
        case '{ $c: Column[t] } => QuotedColumnCompiler.compile(c): Expr[Product => Any]
        case other              => report.errorAndAbort(s"Not a column: ${other.show}", other)
    '{ (row: Product) => ${ Expr.ofTupleFromSeq(compiled.map(f => Expr.betaReduce('{ $f(row) }))) }.asInstanceOf[Product] }

/** The body of a step lambda; the columns inside it do not use the lambda's schema once `$.field` is resolved. */
private[tubes] def stepBody(using Quotes)(step: Expr[Any]): Expr[Any] =
    import quotes.reflect.*
    peel(step.asTerm) match
        case Lambda(_, body) => body.asExpr
        case other           => report.errorAndAbort(s"Expected a step lambda, got ${other.show}", other.pos)

/** The elements of a tuple literal, plain or named. */
private def tupleElements(using Quotes)(tuple: quotes.reflect.Term): List[Expr[Any]] =
    import quotes.reflect.*
    peel(tuple) match
        case Apply(TypeApply(Select(tupleN, "apply"), _), args) if tupleN.symbol.fullName.startsWith("scala.Tuple") =>
            args.map(_.asExpr)
        case other => report.errorAndAbort(s"Expected a tuple of columns, got ${other.show}", other.pos)
