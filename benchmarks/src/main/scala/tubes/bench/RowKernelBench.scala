package tubes.bench

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.*
import tubes.*

/** Rows of the benchmark query: large, ideal-cut diamonds over $1000. */
type Result = (carat: Double, price: Int, volume: Double)

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 5, time = 1)
@Fork(2)
class RowKernelBench:

    var diamonds: Vector[Diamond] = Vector.empty
    var query: Plan[Result]       = null

    @Setup
    def setup(): Unit =
        diamonds = Diamonds.load()
        query = from(diamonds)
            .filter(($.cut === "Ideal") && ($.price > 1000))
            .select(($.carat, $.price, ($.x * $.y * $.z).as("volume")))
            .filter($.volume > 50.0)
        val (expected, actual) = (handWritten(), rowKernel())
        require(expected == actual, s"interpreter disagrees: ${expected.size} vs ${actual.size} rows")

    @Benchmark
    def handWritten(): List[Result] =
        diamonds.iterator
            .filter(d => d.cut == "Ideal" && d.price > 1000)
            .map(d => (carat = d.carat, price = d.price, volume = d.x * d.y * d.z))
            .filter(_.volume > 50.0)
            .toList

    @Benchmark
    def rowKernel(): List[Result] =
        query.materialize(RowKernel())
