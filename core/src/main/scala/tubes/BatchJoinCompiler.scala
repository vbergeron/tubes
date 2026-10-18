package tubes

import scala.quoted.*
import NamedTuple.AnyNamedTuple

/** A join condition compiled for the batch kernel: the left and right columns of its hash keys, in order, each pair of
  * one type; the loops of a key map for those types, unless the kernel's own maps suit them better; and the rest of the
  * condition, checked on the candidate pairs, with its compiled function where it could be compiled.
  */
final class BatchJoin(
    val leftKeys: List[BatchColumn],
    val rightKeys: List[BatchColumn],
    val codec: Option[KeyCodec],
    val residual: Option[Column[Boolean]],
    val residualFn: Option[BatchColumn]
)

/** Compiles a join condition written with `$left` and `$right` for the batch kernel, splitting it as `compileJoin` does;
  * `None` when a key column holds a node the compiler does not handle, and the kernel then interprets the condition.
  */
inline def compileBatchJoin[L <: AnyNamedTuple, R <: AnyNamedTuple](inline step: JoinSchemas[L, R] ?=> Column[Boolean])
    : Option[BatchJoin] = ${ compileBatchJoinImpl[L, R]('step) }

private def compileBatchJoinImpl[L <: AnyNamedTuple: Type, R <: AnyNamedTuple: Type](
    step: Expr[JoinSchemas[L, R] ?=> Column[Boolean]]
)(using Quotes): Expr[Option[BatchJoin]] =
    val (keys, conjuncts) = splitJoin(stepBody(step).asExprOf[Column[Boolean]], joinArity[L])
    val residual          = conjuncts.reduceOption((l, r) => '{ Column.And($l, $r) })
    BatchColumnCompiler.attempt:
        val compiled = keys.map: key =>
            import key.tpe
            (BatchColumnCompiler.function(key.left), BatchColumnCompiler.function(key.right), key.tpe)
        (Expr.ofList(compiled.map(_._1)), Expr.ofList(compiled.map(_._2)), BatchKeyCompiler.codec(compiled.map(_._3)))
    match
        case Some((left, right, codec)) =>
            val (tree, fn) = residual match
                case Some(c) => ('{ Some($c) }, BatchColumnCompiler.compile(c))
                case None    => ('{ None }, '{ None })
            '{ Some(BatchJoin($left, $right, $codec, $tree, $fn)) }
        case None => '{ None }
