package tubes.bench

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.*
import tubes.*

/** `price > 1000 && x * y * z > 50.0` over diamond rows, written with `Column` constructors. */
inline def bigAndPricey: Column[Boolean] =
    Column.And(
        Column.Compare[Int, Int, Int, Boolean](CmpOp.Gt, Column.Ref[Int, "price"]("price", 6), Column.Lit(1000), Ordering.Int),
        Column.Compare[Double, Double, Double, Boolean](
            CmpOp.Gt,
            Column.Arith[Double, Double, Double, Double](
                ArithOp.Times,
                Column.Arith[Double, Double, Double, Double](ArithOp.Times, Column.Ref[Double, "x"]("x", 7), Column.Ref[Double, "y"]("y", 8), Numeric.DoubleIsFractional),
                Column.Ref[Double, "z"]("z", 9),
                Numeric.DoubleIsFractional
            ),
            Column.Lit(50.0),
            Ordering.Double.TotalOrdering
        )
    )

/** Compares column compilers on one predicate, counting matching rows over `Diamond` rows, as a scan yields them. */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 10, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class ColumnBench:

    var rows: Array[Product]           = Array.empty
    val quoted: Product => Boolean     = compileColumn(bigAndPricey)
    val handWrittenPredicate: Product => Boolean = row =>
        val d = row.asInstanceOf[Diamond]
        d.price > 1000 && d.x * d.y * d.z > 50.0

    @Setup
    def setup(): Unit =
        rows = Diamonds.load().toArray
        // Checked outside `count`, so its call site only ever sees the predicate being measured.
        val counts = Seq(handWrittenPredicate, quoted).map(p => rows.count(p))
        require(counts.distinct.size == 1, s"compilers disagree: $counts")

    private def count(p: Product => Boolean): Int =
        var n = 0
        var i = 0
        while i < rows.length do
            if p(rows(i)) then n += 1
            i += 1
        n

    @Benchmark def handWritten(): Int   = count(handWrittenPredicate)
    @Benchmark def quotedMacro(): Int   = count(quoted)
