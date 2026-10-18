package tubes

import scala.quoted.*

/** A column compiled for the batch kernel: computes the column's values over a whole batch. Its array is of the
  * primitive type of the column's static type when there is one (`Int`, `Long`, `Double` or `Boolean`), and an
  * `Array[AnyRef]` of boxed values otherwise.
  */
type BatchColumn = ColumnBatch => Array[?]

/** Compiles the column computed by a step lambda for the batch kernel, as `compileStep` does for the row kernel; `None`
  * when the column holds a node it cannot compile, which the batch kernel then interprets.
  */
inline def compileBatchStep[S, A](inline step: S ?=> Column[A]): Option[BatchColumn] = ${ compileBatchStepImpl('step) }

/** Same as `compileBatchStep`, for each column of a `select` lambda, in order. */
inline def compileBatchProjection[S, C](inline step: S ?=> C): List[Option[BatchColumn]] =
    ${ compileBatchProjectionImpl('step) }

private def compileBatchStepImpl[S: Type, A: Type](step: Expr[S ?=> Column[A]])(using Quotes): Expr[Option[BatchColumn]] =
    BatchColumnCompiler.compile(stepBody(step).asExprOf[Column[A]])

private def compileBatchProjectionImpl[S: Type, C: Type](step: Expr[S ?=> C])(using Quotes)
    : Expr[List[Option[BatchColumn]]] =
    import quotes.reflect.*
    Expr.ofList(tupleElements(stepBody(step).asTerm).map:
        case '{ $c: Column[t] } => BatchColumnCompiler.compile(c)
        case _                  => '{ None })

/** Compiles quoted column trees into one loop over a batch.
  *
  * The interpreter evaluates a tree node by node, each node a loop of its own writing an intermediate array, and
  * picks each loop at run time from the arrays' classes and the `Numeric` or `Ordering` instances. Here the whole tree
  * becomes the body of a single loop: every column it reads is loaded once as an array of its static type, and every
  * node is inlined as scalar code over row `i`. Where an instance is one of the standard ones on `Int`, `Long` or
  * `Double`, its operation is written as the primitive operator, so the loop reads, computes and writes primitives.
  */
private object BatchColumnCompiler:

    /** How values of a static type are held in a batch: one of the primitive arrays, or boxed. */
    enum Repr:
        case IntR, LongR, DoubleR, BooleanR, Boxed

    /** Thrown on a node the compiler does not handle; the column is then left to the interpreter. */
    private final class Unsupported extends Exception(null, null, false, false)

    def compile[A: Type](col: Expr[Column[A]])(using Quotes): Expr[Option[BatchColumn]] =
        try
            val body = function(col)
            '{ Some($body) }
        catch case _: Unsupported => '{ None }

    def repr[A: Type](using Quotes): Repr =
        import quotes.reflect.*
        val t = TypeRepr.of[A].dealias
        if t =:= TypeRepr.of[Int] then Repr.IntR
        else if t =:= TypeRepr.of[Long] then Repr.LongR
        else if t =:= TypeRepr.of[Double] then Repr.DoubleR
        else if t =:= TypeRepr.of[Boolean] then Repr.BooleanR
        else Repr.Boxed

    /** The function computing `col` over a batch. A column that only reads a field returns that field's array. */
    private def function[A: Type](col: Expr[Column[A]])(using Quotes): Expr[BatchColumn] =
        val refs = references(col).distinctBy(_._1)
        '{ (batch: ColumnBatch) =>
            ${
                field(col) match
                    case Some(name) => load(name, repr[A], 'batch)
                    case None       => bind(refs, Map.empty, 'batch)(env => loop(col, env, 'batch))
            }
        }

    /** The field `col` reads as it is, if it is a reference, possibly renamed. */
    private def field(col: Expr[Column[?]])(using Quotes): Option[String] = stripColumn(col) match
        case '{ type n <: String; Column.Ref[t, n]($name, $index) } => name.value
        case '{ type n <: String; Column.Alias[t, n]($c, $name) }   => field(c)
        case _                                                     => None

    /** The fields `col` reads, with how each is held. */
    private def references(col: Expr[Column[?]])(using Quotes): List[(String, Repr)] = stripColumn(col) match
        case '{ type n <: String; Column.Ref[t, n]($name, $index) } =>
            List(name.value.getOrElse(throw Unsupported()) -> repr[t])
        case c => children(c).getOrElse(Nil).flatMap(references)

    /** The array of field `name`, as `repr` holds it: converted if the batch holds it otherwise, e.g. when empty. */
    private def load(name: String, repr: Repr, batch: Expr[ColumnBatch])(using Quotes): Expr[Array[?]] =
        val values = '{ $batch(${ Expr(name) }) }
        repr match
            case Repr.IntR     => '{ BatchColumns.ints($values) }
            case Repr.LongR    => '{ BatchColumns.longs($values) }
            case Repr.DoubleR  => '{ BatchColumns.doubles($values) }
            case Repr.BooleanR => '{ BatchColumns.booleans($values) }
            case Repr.Boxed    => '{ BatchColumns.objects($values) }

    /** Binds the array of each of `refs` to a local value, then builds `body` over them. */
    private def bind(refs: List[(String, Repr)], env: Map[String, Expr[Array[?]]], batch: Expr[ColumnBatch])(
        body: Map[String, Expr[Array[?]]] => Expr[Array[?]]
    )(using Quotes): Expr[Array[?]] = refs match
        case Nil => body(env)
        case (name, repr) :: rest =>
            /** Binds the field's array as an `Array[T]`, then the rest. */
            def let[T: Type]: Expr[Array[?]] =
                '{
                    val a = ${ load(name, repr, batch) }.asInstanceOf[Array[T]]
                    ${ bind(rest, env + (name -> 'a), batch)(body) }
                }
            repr match
                case Repr.IntR     => let[Int]
                case Repr.LongR    => let[Long]
                case Repr.DoubleR  => let[Double]
                case Repr.BooleanR => let[Boolean]
                case Repr.Boxed    => let[AnyRef]

    /** One loop over the rows of `batch`, writing `col` at every row into an array of its representation. */
    private def loop[A: Type](col: Expr[Column[A]], env: Map[String, Expr[Array[?]]], batch: Expr[ColumnBatch])(
        using Quotes
    ): Expr[Array[?]] =
        def at(i: Expr[Int]): Expr[A] = scalar(col, env, i)
        repr[A] match
            case Repr.IntR =>
                '{
                    val n   = $batch.size
                    val out = new Array[Int](n)
                    var i   = 0
                    while i < n do
                        val row = i
                        out(row) = ${ at('row).asExprOf[Int] }
                        i += 1
                    out
                }
            case Repr.LongR =>
                '{
                    val n   = $batch.size
                    val out = new Array[Long](n)
                    var i   = 0
                    while i < n do
                        val row = i
                        out(row) = ${ at('row).asExprOf[Long] }
                        i += 1
                    out
                }
            case Repr.DoubleR =>
                '{
                    val n   = $batch.size
                    val out = new Array[Double](n)
                    var i   = 0
                    while i < n do
                        val row = i
                        out(row) = ${ at('row).asExprOf[Double] }
                        i += 1
                    out
                }
            case Repr.BooleanR =>
                '{
                    val n   = $batch.size
                    val out = new Array[Boolean](n)
                    var i   = 0
                    while i < n do
                        val row = i
                        out(row) = ${ at('row).asExprOf[Boolean] }
                        i += 1
                    out
                }
            case Repr.Boxed =>
                '{
                    val n   = $batch.size
                    val out = new Array[AnyRef](n)
                    var i   = 0
                    while i < n do
                        val row = i
                        out(row) = ${ at('row) }.asInstanceOf[AnyRef]
                        i += 1
                    out
                }

    /** Whether `instance` is the standard given named `name`, such as `scala.math.Numeric.IntIsIntegral`. */
    private def isStandard(instance: Expr[Any], names: String*)(using Quotes): Boolean =
        import quotes.reflect.*
        val sym = peel(instance.asTerm).symbol
        !sym.isNoSymbol && names.contains(sym.fullName)

    private val IntNumeric    = "scala.math.Numeric$.IntIsIntegral"
    private val LongNumeric   = "scala.math.Numeric$.LongIsIntegral"
    private val DoubleNumeric = "scala.math.Numeric$.DoubleIsFractional"
    private val IntOrdering   = "scala.math.Ordering$.Int"
    private val LongOrdering  = "scala.math.Ordering$.Long"
    /** The `Double` orderings comparing as `java.lang.Double.compare` does, the given one among them. */
    private val DoubleTotalOrderings =
        Seq("scala.math.Ordering$.DeprecatedDoubleOrdering", "scala.math.Ordering$.Double$.TotalOrdering")
    /** The `Double` ordering comparing as the operators do. */
    private val DoubleIeeeOrdering = "scala.math.Ordering$.Double$.IeeeOrdering"

    /** The value of `col` at row `i`, the arrays of the fields it reads being bound in `env`. */
    private def scalar[A: Type](col: Expr[Column[A]], env: Map[String, Expr[Array[?]]], i: Expr[Int])(
        using Quotes
    ): Expr[A] =
        def sub[B: Type](c: Expr[Column[B]]): Expr[B] = scalar(c, env, i)
        stripColumn(col) match
            case '{ type n <: String; Column.Ref[A, n]($name, $index) } =>
                val values = env(name.value.getOrElse(throw Unsupported()))
                (repr[A] match
                    case Repr.IntR     => '{ ${ values.asExprOf[Array[Int]] }($i) }
                    case Repr.LongR    => '{ ${ values.asExprOf[Array[Long]] }($i) }
                    case Repr.DoubleR  => '{ ${ values.asExprOf[Array[Double]] }($i) }
                    case Repr.BooleanR => '{ ${ values.asExprOf[Array[Boolean]] }($i) }
                    case Repr.Boxed    => '{ ${ values.asExprOf[Array[AnyRef]] }($i).asInstanceOf[A] }
                ).asExprOf[A]
            case '{ type n <: String; Column.Alias[A, n]($c, $name) } => sub(c)
            case '{ Column.Lit[A]($value) }                         => value
            case '{ Column.Arith[A]($op, $l, $r, $num) } =>
                val (a, b) = (sub(l), sub(r))
                def primitive[P: Type](plus: Expr[P], minus: Expr[P], times: Expr[P]): Expr[A] = (op match
                    case '{ ArithOp.Plus }  => plus
                    case '{ ArithOp.Minus } => minus
                    case '{ ArithOp.Times } => times
                ).asExprOf[A]
                (repr[A], a, b) match
                    case (Repr.IntR, '{ $x: Int }, '{ $y: Int }) if isStandard(num, IntNumeric) =>
                        primitive('{ $x + $y }, '{ $x - $y }, '{ $x * $y })
                    case (Repr.LongR, '{ $x: Long }, '{ $y: Long }) if isStandard(num, LongNumeric) =>
                        primitive('{ $x + $y }, '{ $x - $y }, '{ $x * $y })
                    case (Repr.DoubleR, '{ $x: Double }, '{ $y: Double }) if isStandard(num, DoubleNumeric) =>
                        primitive('{ $x + $y }, '{ $x - $y }, '{ $x * $y })
                    case _ =>
                        op match
                            case '{ ArithOp.Plus }  => '{ $num.plus($a, $b) }
                            case '{ ArithOp.Minus } => '{ $num.minus($a, $b) }
                            case '{ ArithOp.Times } => '{ $num.times($a, $b) }
                            case _                  => throw Unsupported()
            case '{ Column.Compare[t]($op, $l, $r, $ord) } =>
                val (a, b) = (sub(l), sub(r))
                // The given `Double` ordering is a total one, which orders NaN: it compares as `Double.compare` does, not
                // as the operators do.
                val primitive: Option[(Expr[Boolean], Expr[Boolean], Expr[Boolean], Expr[Boolean])] = (a, b) match
                    case ('{ $x: Int }, '{ $y: Int }) if isStandard(ord, IntOrdering) =>
                        Some(('{ $x < $y }, '{ $x <= $y }, '{ $x > $y }, '{ $x >= $y }))
                    case ('{ $x: Long }, '{ $y: Long }) if isStandard(ord, LongOrdering) =>
                        Some(('{ $x < $y }, '{ $x <= $y }, '{ $x > $y }, '{ $x >= $y }))
                    case ('{ $x: Double }, '{ $y: Double }) if isStandard(ord, DoubleIeeeOrdering) =>
                        Some(('{ $x < $y }, '{ $x <= $y }, '{ $x > $y }, '{ $x >= $y }))
                    case ('{ $x: Double }, '{ $y: Double }) if isStandard(ord, DoubleTotalOrderings*) =>
                        val c = '{ java.lang.Double.compare($x, $y) }
                        Some(('{ $c < 0 }, '{ $c <= 0 }, '{ $c > 0 }, '{ $c >= 0 }))
                    case _ => None
                val (lt, le, gt, ge) = primitive.getOrElse(
                    ('{ $ord.lt($a, $b) }, '{ $ord.lteq($a, $b) }, '{ $ord.gt($a, $b) }, '{ $ord.gteq($a, $b) })
                )
                (op match
                    case '{ CmpOp.Lt } => lt
                    case '{ CmpOp.Le } => le
                    case '{ CmpOp.Gt } => gt
                    case '{ CmpOp.Ge } => ge
                    case _             => throw Unsupported()
                ).asExprOf[A]
            case '{ Column.Equal[t]($l, $r) } => '{ ${ sub(l) } == ${ sub(r) } }.asExprOf[A]
            case '{ Column.And($l, $r) }      => '{ ${ sub(l) } && ${ sub(r) } }.asExprOf[A]
            case '{ Column.Or($l, $r) }       => '{ ${ sub(l) } || ${ sub(r) } }.asExprOf[A]
            case '{ Column.Not($c) }          => '{ !${ sub(c) } }.asExprOf[A]
            case '{ Column.Default[A]($c, $value) } =>
                '{
                    ${ sub(c) } match
                        case Some(a) => a
                        case _       => ${ sub(value) }
                }
            case '{ Column.Div[A]($l, $r, $frac) } =>
                (sub(l), sub(r)) match
                    case ('{ $x: Double }, '{ $y: Double }) if isStandard(frac, DoubleNumeric) => '{ $x / $y }.asExprOf[A]
                    case (a, b)                                                               => '{ $frac.div($a, $b) }
            case '{ Column.Quot[A]($l, $r, $int) } =>
                (sub(l), sub(r)) match
                    case ('{ $x: Int }, '{ $y: Int }) if isStandard(int, IntNumeric)    => '{ $x / $y }.asExprOf[A]
                    case ('{ $x: Long }, '{ $y: Long }) if isStandard(int, LongNumeric) => '{ $x / $y }.asExprOf[A]
                    case (a, b)                                                         => '{ $int.quot($a, $b) }
            case '{ Column.ToDouble[t]($c, $num) } =>
                (sub(c) match
                    case '{ $x: Int } if isStandard(num, IntNumeric)       => '{ $x.toDouble }
                    case '{ $x: Long } if isStandard(num, LongNumeric)     => '{ $x.toDouble }
                    case '{ $x: Double } if isStandard(num, DoubleNumeric) => x
                    case x                                                 => '{ $num.toDouble($x) }
                ).asExprOf[A]
            case _ => throw Unsupported()

/** Reads the columns of a batch as the arrays compiled columns expect, converting those held otherwise. */
object BatchColumns:
    def ints(a: Array[?]): Array[Int]          = Vectors.unbox[Int](a)
    def longs(a: Array[?]): Array[Long]        = Vectors.unbox[Long](a)
    def doubles(a: Array[?]): Array[Double]    = Vectors.unbox[Double](a)
    def booleans(a: Array[?]): Array[Boolean]  = Vectors.unbox[Boolean](a)
    def objects(a: Array[?]): Array[AnyRef] = (a: Any) match
        case a: Array[AnyRef] => a
        case _                => Vectors.build[AnyRef](a.length)(Vectors.get(a, _).asInstanceOf[AnyRef])
