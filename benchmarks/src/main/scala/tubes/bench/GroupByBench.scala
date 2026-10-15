package tubes.bench

import java.util.concurrent.TimeUnit
import scala.jdk.CollectionConverters.*
import org.openjdk.jmh.annotations.*
import tubes.*

type CutStats   = (cut: String, n: Long, total: Int, avgCarat: Double)
type GradeRange = (cut: String, color: String, clarity: String, low: Int, high: Int, n: Long)
type ColorStats = (color: String, avgPrice: Double, widest: Double)

/** Aggregations over diamond rows, against hand-written loops folding each group into a mutable accumulator. */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 5, time = 1)
@Fork(2)
class GroupByBench:

    var diamonds: Vector[Diamond] = Vector.empty

    /** 5 groups on one key, with a count, a sum and an average. */
    var byCut: Plan[CutStats] = null

    /** 276 groups on a three-column key, with a minimum, a maximum and a count. */
    var byGrade: Plan[GradeRange] = null

    /** A filter, then 7 groups on one key, with an average and a maximum of `Double`s. */
    var largeByColor: Plan[ColorStats] = null

    @Setup
    def setup(): Unit =
        diamonds = Diamonds.load()
        byCut = from(diamonds)
            .groupBy($.cut)((count.as("n"), $.price.sum.as("total"), $.carat.avg.as("avgCarat")))
        byGrade = from(diamonds)
            .groupBy(($.cut, $.color, $.clarity))(($.price.min.as("low"), $.price.max.as("high"), count.as("n")))
        largeByColor = from(diamonds)
            .filter($.carat > 1.0)
            .groupBy($.color)(($.price.avg.as("avgPrice"), $.x.max.as("widest")))
        check("byCut", handWrittenByCut(), rowKernelByCut())
        check("byGrade", handWrittenByGrade(), rowKernelByGrade())
        check("largeByColor", handWrittenLargeByColor(), rowKernelLargeByColor())

    private def check(name: String, expected: List[?], actual: List[?]): Unit =
        require(expected.nonEmpty, s"$name: no rows")
        require(expected == actual, s"$name: kernel disagrees: $expected vs $actual")

    private final class CutAcc(var n: Long, var total: Int, var carats: Double)

    @Benchmark
    def handWrittenByCut(): List[CutStats] =
        val groups = java.util.LinkedHashMap[String, CutAcc]()
        for d <- diamonds do
            var acc = groups.get(d.cut)
            if acc == null then
                acc = CutAcc(0, 0, 0)
                groups.put(d.cut, acc)
            acc.n += 1
            acc.total += d.price
            acc.carats += d.carat
        groups.asScala.iterator.map((cut, a) => (cut = cut, n = a.n, total = a.total, avgCarat = a.carats / a.n)).toList

    @Benchmark
    def rowKernelByCut(): List[CutStats] = byCut.materialize(RowKernel(List))

    private final class GradeAcc(var low: Int, var high: Int, var n: Long)

    @Benchmark
    def handWrittenByGrade(): List[GradeRange] =
        val groups = java.util.LinkedHashMap[(String, String, String), GradeAcc]()
        for d <- diamonds do
            val key = (d.cut, d.color, d.clarity)
            val acc = groups.get(key)
            if acc == null then groups.put(key, GradeAcc(d.price, d.price, 1))
            else
                if d.price < acc.low then acc.low = d.price
                if d.price > acc.high then acc.high = d.price
                acc.n += 1
        groups.asScala.iterator
            .map((k, a) => (cut = k._1, color = k._2, clarity = k._3, low = a.low, high = a.high, n = a.n))
            .toList

    @Benchmark
    def rowKernelByGrade(): List[GradeRange] = byGrade.materialize(RowKernel(List))

    private final class ColorAcc(var prices: Double, var n: Long, var widest: Double)

    @Benchmark
    def handWrittenLargeByColor(): List[ColorStats] =
        val groups = java.util.LinkedHashMap[String, ColorAcc]()
        for d <- diamonds if d.carat > 1.0 do
            val acc = groups.get(d.color)
            if acc == null then groups.put(d.color, ColorAcc(d.price, 1, d.x))
            else
                acc.prices += d.price
                acc.n += 1
                if d.x > acc.widest then acc.widest = d.x
        groups.asScala.iterator.map((color, a) => (color = color, avgPrice = a.prices / a.n, widest = a.widest)).toList

    @Benchmark
    def rowKernelLargeByColor(): List[ColorStats] = largeByColor.materialize(RowKernel(List))
