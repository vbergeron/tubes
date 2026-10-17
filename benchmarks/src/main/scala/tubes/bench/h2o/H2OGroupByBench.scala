package tubes.bench.h2o

import java.util.concurrent.TimeUnit
import scala.jdk.CollectionConverters.*
import scala.reflect.ClassTag
import org.openjdk.jmh.annotations.*
import tubes.*

type Q1  = (id1: String, v1: Int)
type Q2  = (id1: String, id2: String, v1: Int)
type Q3  = (id3: String, v1: Int, v3: Double)
type Q4  = (id4: Int, v1: Double, v2: Double, v3: Double)
type Q5  = (id6: Int, v1: Int, v2: Int, v3: Double)
type Q7  = (id3: String, range_v1_v2: Int)
type Q10 = (id1: String, id2: String, id3: String, id4: Int, id5: Int, id6: Int, v3: Double, count: Long)

/** The group-by questions of the H2O.ai db-benchmark (https://h2oai.github.io/db-benchmark/), over `n` generated rows.
  * Each supported question runs on the row kernel and as a hand-written loop folding groups into a `LinkedHashMap`, both collecting into an `IArray`;
  * setup checks both give the same rows.
  *
  * Questions tubes cannot express yet are kept as placeholders at the end, with the syntax they would use.
  */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 2)
@Measurement(iterations = 5, time = 2)
@Fork(2)
class H2OGroupByBench:

    @Param(Array("1000000"))
    var n: Int = 0

    var rows: Vector[GroupRow] = Vector.empty

    var q1: Plan[Q1]   = null
    var q2: Plan[Q2]   = null
    var q3: Plan[Q3]   = null
    var q4: Plan[Q4]   = null
    var q5: Plan[Q5]   = null
    var q7: Plan[Q7]   = null
    var q10: Plan[Q10] = null

    @Setup
    def setup(): Unit =
        rows = H2OData.groupBy(n)
        q1 = from(rows).groupBy($.id1)($.v1.sum.as("v1"))
        q2 = from(rows).groupBy(($.id1, $.id2))($.v1.sum.as("v1"))
        q3 = from(rows).groupBy($.id3)(($.v1.sum.as("v1"), $.v3.avg.as("v3")))
        q4 = from(rows).groupBy($.id4)(($.v1.avg.as("v1"), $.v2.avg.as("v2"), $.v3.avg.as("v3")))
        q5 = from(rows).groupBy($.id6)(($.v1.sum.as("v1"), $.v2.sum.as("v2"), $.v3.sum.as("v3")))
        q7 = from(rows).groupBy($.id3)(($.v1.max - $.v2.min).as("range_v1_v2"))
        q10 = from(rows).groupBy(($.id1, $.id2, $.id3, $.id4, $.id5, $.id6))(($.v3.sum.as("v3"), count.as("count")))
        check("q1", handWrittenQ1(), rowKernelQ1())
        check("q2", handWrittenQ2(), rowKernelQ2())
        check("q3", handWrittenQ3(), rowKernelQ3())
        check("q4", handWrittenQ4(), rowKernelQ4())
        check("q5", handWrittenQ5(), rowKernelQ5())
        check("q7", handWrittenQ7(), rowKernelQ7())
        check("q10", handWrittenQ10(), rowKernelQ10())

    private def check[R](name: String, expected: IArray[R], actual: IArray[R]): Unit =
        require(expected.nonEmpty, s"$name: no rows")
        require(expected.sameElements(actual), s"$name: kernel disagrees on ${expected.size} groups")

    /** Folds every row into the accumulator of its group, created by `init`; groups keep their first-seen order. */
    private inline def fold[K, A](inline key: GroupRow => K, inline init: GroupRow => A)(inline add: (A, GroupRow) => Unit)
        : java.util.LinkedHashMap[K, A] =
        val groups = java.util.LinkedHashMap[K, A]()
        for r <- rows do
            val k   = key(r)
            val acc = groups.get(k)
            if acc == null then
                val fresh = init(r)
                add(fresh, r)
                groups.put(k, fresh)
            else add(acc, r)
        groups

    extension [A: ClassTag](groups: Iterator[A]) private def toIArray: IArray[A] = IArray.unsafeFromArray(groups.toArray)

    private final class IntSum(var v: Int)
    private final class SumMean(var v1: Int, var v3: Double, var n: Long)
    private final class Means(var v1: Long, var v2: Long, var v3: Double, var n: Long)
    private final class Sums(var v1: Int, var v2: Int, var v3: Double)
    private final class SumCount(var v3: Double, var n: Long)
    private final class MaxMin(var v1: Int, var v2: Int)

    // q1: sum v1 by id1 (100 groups)

    @Benchmark def rowKernelQ1(): IArray[Q1] = q1.materialize(RowKernel(IArray))

    @Benchmark def handWrittenQ1(): IArray[Q1] =
        fold(_.id1, _ => IntSum(0))((a, r) => a.v += r.v1).asScala.iterator.map((k, a) => (id1 = k, v1 = a.v)).toIArray

    // q2: sum v1 by id1, id2 (10k groups)

    @Benchmark def rowKernelQ2(): IArray[Q2] = q2.materialize(RowKernel(IArray))

    @Benchmark def handWrittenQ2(): IArray[Q2] =
        fold(r => (r.id1, r.id2), _ => IntSum(0))((a, r) => a.v += r.v1)
            .asScala.iterator.map((k, a) => (id1 = k._1, id2 = k._2, v1 = a.v)).toIArray

    // q3: sum v1, mean v3 by id3 (n / 100 groups)

    @Benchmark def rowKernelQ3(): IArray[Q3] = q3.materialize(RowKernel(IArray))

    @Benchmark def handWrittenQ3(): IArray[Q3] =
        fold(_.id3, _ => SumMean(0, 0, 0)): (a, r) =>
            a.v1 += r.v1
            a.v3 += r.v3
            a.n += 1
        .asScala.iterator.map((k, a) => (id3 = k, v1 = a.v1, v3 = a.v3 / a.n)).toIArray

    // q4: mean v1, v2, v3 by id4 (100 groups)

    @Benchmark def rowKernelQ4(): IArray[Q4] = q4.materialize(RowKernel(IArray))

    @Benchmark def handWrittenQ4(): IArray[Q4] =
        fold(_.id4, _ => Means(0, 0, 0, 0)): (a, r) =>
            a.v1 += r.v1
            a.v2 += r.v2
            a.v3 += r.v3
            a.n += 1
        .asScala.iterator
            .map((k, a) => (id4 = k, v1 = a.v1.toDouble / a.n, v2 = a.v2.toDouble / a.n, v3 = a.v3 / a.n))
            .toIArray

    // q5: sum v1, v2, v3 by id6 (n / 100 groups)

    @Benchmark def rowKernelQ5(): IArray[Q5] = q5.materialize(RowKernel(IArray))

    @Benchmark def handWrittenQ5(): IArray[Q5] =
        fold(_.id6, _ => Sums(0, 0, 0)): (a, r) =>
            a.v1 += r.v1
            a.v2 += r.v2
            a.v3 += r.v3
        .asScala.iterator.map((k, a) => (id6 = k, v1 = a.v1, v2 = a.v2, v3 = a.v3)).toIArray

    // q7: max v1 - min v2 by id3 (n / 100 groups)

    @Benchmark def rowKernelQ7(): IArray[Q7] = q7.materialize(RowKernel(IArray))

    @Benchmark def handWrittenQ7(): IArray[Q7] =
        fold(_.id3, r => MaxMin(r.v1, r.v2)): (a, r) =>
            if r.v1 > a.v1 then a.v1 = r.v1
            if r.v2 < a.v2 then a.v2 = r.v2
        .asScala.iterator.map((k, a) => (id3 = k, range_v1_v2 = a.v1 - a.v2)).toIArray

    // q10: sum v3, count by id1..id6 (almost one group per row)

    @Benchmark def rowKernelQ10(): IArray[Q10] = q10.materialize(RowKernel(IArray))

    @Benchmark def handWrittenQ10(): IArray[Q10] =
        fold(r => (r.id1, r.id2, r.id3, r.id4, r.id5, r.id6), _ => SumCount(0, 0)): (a, r) =>
            a.v3 += r.v3
            a.n += 1
        .asScala.iterator
            .map((k, a) => (id1 = k._1, id2 = k._2, id3 = k._3, id4 = k._4, id5 = k._5, id6 = k._6, v3 = a.v3, count = a.n))
            .toIArray

    // Placeholders: questions tubes cannot express yet, with the syntax they would use.
    //
    // q6: median v3, sd v3 by id4, id5 (10k groups). Needs `median` and `sd` aggregates, which keep or summarise
    //     every value of a group rather than a constant-size accumulator.
    //       from(rows).groupBy(($.id4, $.id5))(($.v3.median.as("median_v3"), $.v3.sd.as("sd_v3")))
    //
    // q8: largest two v3 by id6 (n / 100 groups, two rows each). Needs top-n per group, producing several rows per
    //     group, or ordering plus a per-group take.
    //       from(rows).groupBy($.id6)($.v3.top(2).as("largest2_v3"))
    //
    // q9: squared correlation of v1 and v2 by id2, id4 (10k groups). Needs a two-column aggregate such as `corr`, and
    //     arithmetic over it.
    //       from(rows).groupBy(($.id2, $.id4))(corr($.v1, $.v2).pow(2).as("r2"))
