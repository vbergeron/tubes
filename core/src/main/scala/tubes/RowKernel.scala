package tubes

import NamedTuple.AnyNamedTuple

/** Row-based kernel: streams a plan's rows through one iterator pipeline, using the row functions compiled into its
  * nodes, and collects the result.
  */
final class RowKernel[Out <: AnyNamedTuple] extends Plan.Compiler[Out]:
    type A = List[Out]

    def compile(plan: Plan[Out]): List[Out] =
        val result = RowKernel.rows(plan)
        (if RowKernel.yieldsTuples(plan) then result else result.map(Tuple.fromProduct)).toList.asInstanceOf[List[Out]]

object RowKernel:
    /** Rows produced by `plan`, in whatever representation its scan or last select left them. */
    private def rows(plan: Plan[?]): Iterator[Product] = plan match
        case s: Scan[?] =>
            s.source match
                case Some(src) => src.scan
                case None      => sys.error(s"RowKernel has no data bound for $s")
        case f: FilterSpec[?] => rows(f.input).filter(f.compiled)
        case s: SelectSpec[?] => rows(s.input).map(s.compiled)

    /** Whether `plan`'s rows are already `Out` tuples; otherwise they are still a source's own values. */
    private def yieldsTuples(plan: Plan[?]): Boolean = plan match
        case _: Scan[?]       => false
        case f: FilterSpec[?] => yieldsTuples(f.input)
        case _: SelectSpec[?] => true
