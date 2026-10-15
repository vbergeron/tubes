package tubes.bench

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.*
import tubes.*

/** A rank for each diamond cut, from Fair (1) to Ideal (5). */
case class Cut(cutName: String, cutRank: Int)

/** A minimum price for each cut, color and clarity combination. */
case class Grade(gradeCut: String, gradeColor: String, gradeClarity: String, minPrice: Int)

object Grades:
    val cutNames   = Vector("Fair", "Good", "Very Good", "Premium", "Ideal")
    val colors     = Vector("D", "E", "F", "G", "H", "I", "J")
    val clarities  = Vector("I1", "SI2", "SI1", "VS2", "VS1", "VVS2", "VVS1", "IF")

    val cuts: Vector[Cut] = cutNames.zipWithIndex.map((name, i) => Cut(name, i + 1))

    val grades: Vector[Grade] =
        for
            (cut, i)     <- cutNames.zipWithIndex
            (color, j)   <- colors.zipWithIndex
            (clarity, k) <- clarities.zipWithIndex
        yield Grade(cut, color, clarity, 500 + 37 * ((i * 7 + j) * 8 + k))

type CutResult   = (carat: Double, price: Int, cutRank: Int)
type GradeResult = (carat: Double, price: Int, minPrice: Int)
type RankResult  = (cutRank: Int, price: Int)

/** Hash joins over diamond rows, against hand-written joins building the same hash tables on the right side. */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 5, time = 1)
@Fork(2)
class JoinBench:

    var diamonds: Vector[Diamond] = Vector.empty

    /** Diamonds probing a 5-row table on one key, then filtered on a right column. */
    var smallBuild: Plan[CutResult] = null

    /** Diamonds probing a 280-row table on a three-column key, with a residual comparison across both sides. */
    var compositeKey: Plan[GradeResult] = null

    /** A 5-row table probing a hash table built over every diamond. */
    var largeBuild: Plan[RankResult] = null

    @Setup
    def setup(): Unit =
        diamonds = Diamonds.load()
        smallBuild = from(diamonds)
            .join(from(Grades.cuts))
            .on($left.cut === $right.cutName)
            .filter($.cutRank >= 4)
            .select(($.carat, $.price, $.cutRank))
        compositeKey = from(diamonds)
            .join(from(Grades.grades))
            .on(
                ($left.cut === $right.gradeCut) && ($left.color === $right.gradeColor) &&
                    ($left.clarity === $right.gradeClarity) && ($left.price > $right.minPrice)
            )
            .select(($.carat, $.price, $.minPrice))
        largeBuild = from(Grades.cuts)
            .join(from(diamonds))
            .on($left.cutName === $right.cut)
            .select(($.cutRank, $.price))
        check("smallBuild", handWrittenSmallBuild(), rowKernelSmallBuild())
        check("compositeKey", handWrittenCompositeKey(), rowKernelCompositeKey())
        check("largeBuild", handWrittenLargeBuild(), rowKernelLargeBuild())

    private def check(name: String, expected: List[?], actual: List[?]): Unit =
        require(expected.nonEmpty, s"$name: no rows")
        require(expected == actual, s"$name: interpreter disagrees: ${expected.size} vs ${actual.size} rows")

    @Benchmark
    def handWrittenSmallBuild(): List[CutResult] =
        val table = Grades.cuts.groupBy(_.cutName)
        diamonds.iterator
            .flatMap(d => table.getOrElse(d.cut, Nil).iterator.map(c => (d, c)))
            .filter((_, c) => c.cutRank >= 4)
            .map((d, c) => (carat = d.carat, price = d.price, cutRank = c.cutRank))
            .toList

    @Benchmark
    def rowKernelSmallBuild(): List[CutResult] =
        smallBuild.materialize(RowKernel())

    @Benchmark
    def handWrittenCompositeKey(): List[GradeResult] =
        val table = Grades.grades.groupBy(g => (g.gradeCut, g.gradeColor, g.gradeClarity))
        diamonds.iterator
            .flatMap(d => table.getOrElse((d.cut, d.color, d.clarity), Nil).iterator.map(g => (d, g)))
            .filter((d, g) => d.price > g.minPrice)
            .map((d, g) => (carat = d.carat, price = d.price, minPrice = g.minPrice))
            .toList

    @Benchmark
    def rowKernelCompositeKey(): List[GradeResult] =
        compositeKey.materialize(RowKernel())

    @Benchmark
    def handWrittenLargeBuild(): List[RankResult] =
        val table = diamonds.groupBy(_.cut)
        Grades.cuts.iterator
            .flatMap(c => table.getOrElse(c.cutName, Nil).iterator.map(d => (cutRank = c.cutRank, price = d.price)))
            .toList

    @Benchmark
    def rowKernelLargeBuild(): List[RankResult] =
        largeBuild.materialize(RowKernel())
