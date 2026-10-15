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
        case '{ Column.Arith[b, l, r, A]($op, $lc, $rc, $num, $lift) } =>
            lifted[l, r, b, b, A](typed(lc), typed(rc)): (x, y) =>
                op match
                    case '{ ArithOp.Plus }  => '{ $num.plus($x, $y) }
                    case '{ ArithOp.Minus } => '{ $num.minus($x, $y) }
                    case '{ ArithOp.Times } => '{ $num.times($x, $y) }
        case '{ Column.Compare[b, l, r, A]($op, $lc, $rc, $ord, $lift) } =>
            lifted[l, r, b, Boolean, A](typed(lc), typed(rc)): (x, y) =>
                op match
                    case '{ CmpOp.Lt } => '{ $ord.lt($x, $y) }
                    case '{ CmpOp.Le } => '{ $ord.lteq($x, $y) }
                    case '{ CmpOp.Gt } => '{ $ord.gt($x, $y) }
                    case '{ CmpOp.Ge } => '{ $ord.gteq($x, $y) }
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
        case '{ Column.IsNone[t]($c) }               =>
            val f = typed(c)
            '{ (row: Product) => $f(row).isEmpty }.asExprOf[Product => A]
        case _ => quotes.reflect.report.errorAndAbort(s"Cannot compile column expression ${expr.show}", expr)

    /** Applies `op` to the values read by `fl` and `fr`, which hold `V` values, possibly as `Option[V]`. When either does,
      * the result `O` is an `Option[B]`, `None` where either value is missing; otherwise it is `B`.
      */
    private def lifted[L: Type, R: Type, V: Type, B: Type, O: Type](fl: Expr[Product => L], fr: Expr[Product => R])(
        op: (Expr[V], Expr[V]) => Expr[B]
    ): Expr[Product => O] =
        val (optL, optR) = (isOption[L], isOption[R])
        /** Reads `f` on `row` as a `V`, unwrapping an `Option[V]` and yielding `None` where it is missing. */
        def read[T: Type](f: Expr[Product => T], opt: Boolean, row: Expr[Product])(k: Expr[V] => Expr[O]): Expr[O] =
            val value = Expr.betaReduce('{ $f($row) })
            if opt then
                '{
                    $value.asInstanceOf[Option[V]] match
                        case Some(v) => ${ k('v) }
                        case None    => None.asInstanceOf[O]
                }
            else k('{ $value.asInstanceOf[V] })
        '{ (row: Product) =>
            ${
                read(fl, optL, 'row): x =>
                    read(fr, optR, 'row): y =>
                        if optL || optR then '{ Some(${ op(x, y) }).asInstanceOf[O] } else '{ ${ op(x, y) }.asInstanceOf[O] }
            }
        }

    private def isOption[T: Type]: Boolean =
        import quotes.reflect.*
        TypeRepr.of[T].dealias.simplified <:< TypeRepr.of[Option[Any]]

/** Drops what inlining and `Selectable` leave around an expression: inlined wrappers, ascriptions and casts. */
private[tubes] def peel(using Quotes)(term: quotes.reflect.Term): quotes.reflect.Term =
    import quotes.reflect.*
    term match
        case Inlined(_, Nil, body)                                         => peel(body)
        case Typed(body, _)                                                => peel(body)
        case Block(Nil, body)                                              => peel(body)
        case TypeApply(Select(body, "asInstanceOf" | "$asInstanceOf$"), _) => peel(body)
        case _                                                             => term

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
