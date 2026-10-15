package tubes.bench

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.*
import tubes.*

/** Diamond rows, as a scan of `Seq[Diamond]` describes them. */
type DiamondRow = NamedTuple.From[Diamond]

/** Compares the compiled form of one predicate with a hand-written one, counting matching `Diamond` rows. */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 10, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class ColumnBench:

    var rows: Array[Product] = Array.empty

    /** `price > 1000 && x * y * z > 50.0`, compiled as a `filter` would compile it. */
    val quoted: Product => Boolean =
        compileStep[Schema[DiamondRow], Boolean](($.price > 1000) && ($.x * $.y * $.z > 50.0))

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

    @Benchmark def handWritten(): Int = count(handWrittenPredicate)
    @Benchmark def quotedMacro(): Int = count(quoted)
