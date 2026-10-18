package tubes.bench

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import tubes.*

case class Gem(carat: Double, depth: Double, price: Int, x: Double, y: Double, z: Double, cut: String)

/** Compares the batch kernel interpreting column trees with the same kernel running the loops compiled at the call
  * site, on a filter and a select over a columnar table, and both with hand-written loops over the same arrays.
  */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 5, time = 1)
@Fork(1)
class BatchExprBench:

    @Param(Array("1000000"))
    var n: Int = 0

    var gems: Columns[NamedTuple.From[Gem]] = null

    /** A predicate over four columns, with three multiplications. */
    def filtered = from(gems).filter(($.price > 1000) && ($.x * $.y * $.z > 50.0))

    /** Five arithmetic columns, one of them reading the others' fields. */
    def selected = from(gems).select((
        ($.price * 2 + 1).as("p"),
        ($.carat * 100.0).as("c"),
        ($.x * $.y * $.z).as("volume"),
        (($.depth - 60.0) * ($.depth - 60.0)).as("dev"),
        ($.price.toDouble / $.carat).as("perCarat")
    ))

    @Setup
    def setup(): Unit =
        val rng  = scala.util.Random(42)
        val cuts = Array("Fair", "Good", "Very Good", "Premium", "Ideal")
        val rows = Vector.fill(n):
            val carat = 0.2 + rng.nextDouble() * 3
            Gem(carat, 55 + rng.nextDouble() * 10, 300 + rng.nextInt(18000), 3 + rng.nextDouble() * 6,
                3 + rng.nextDouble() * 6, 2 + rng.nextDouble() * 4, cuts(rng.nextInt(cuts.length)))
        gems = Columns.from(rows)
        // Both ways must agree, and with the hand-written loops.
        for plan <- List(filtered, selected) do
            val counts = List(true, false).map(c => BatchKernel.batches(plan, BatchKernel.DefaultBatchSize, c).map(_.size).sum)
            require(counts.distinct.size == 1, s"kernels disagree: $counts")
        require(BatchKernel.batches(filtered).map(_.size).sum == filterByHand(_ => ()), "hand-written filter disagrees")

    private def run(plan: Plan[?], compiled: Boolean, bh: Blackhole): Int =
        var rows = 0
        for batch <- BatchKernel.batches(plan, BatchKernel.DefaultBatchSize, compiled) do
            bh.consume(batch)
            rows += batch.size
        rows

    @Benchmark def filterInterpreted(bh: Blackhole): Int = run(filtered, compiled = false, bh)
    @Benchmark def filterCompiled(bh: Blackhole): Int    = run(filtered, compiled = true, bh)
    @Benchmark def selectInterpreted(bh: Blackhole): Int = run(selected, compiled = false, bh)
    @Benchmark def selectCompiled(bh: Blackhole): Int    = run(selected, compiled = true, bh)

    /** The filter over each batch in one loop, then the matching rows of every column, as the kernel gathers them. */
    @Benchmark def handWrittenFilter(bh: Blackhole): Int = filterByHand(bh.consume)

    private def filterByHand(consume: ColumnBatch => Unit): Int =
        var rows = 0
        for batch <- gems.batches do
            val price = batch("price").asInstanceOf[Array[Int]]
            val (x, y, z) =
                (batch("x").asInstanceOf[Array[Double]], batch("y").asInstanceOf[Array[Double]], batch("z").asInstanceOf[Array[Double]])
            val size    = batch.size
            val indices = new Array[Int](size)
            var count   = 0
            var i       = 0
            while i < size do
                if price(i) > 1000 && x(i) * y(i) * z(i) > 50.0 then
                    indices(count) = i
                    count += 1
                i += 1
            consume(if count == size then batch else batch.gather(indices, count))
            rows += count
        rows

    /** The five columns over each batch, each in a loop of its own. */
    @Benchmark def handWrittenSelect(bh: Blackhole): Int =
        var rows = 0
        for batch <- gems.batches do
            val size  = batch.size
            val price = batch("price").asInstanceOf[Array[Int]]
            val carat = batch("carat").asInstanceOf[Array[Double]]
            val depth = batch("depth").asInstanceOf[Array[Double]]
            val (x, y, z) =
                (batch("x").asInstanceOf[Array[Double]], batch("y").asInstanceOf[Array[Double]], batch("z").asInstanceOf[Array[Double]])
            val p        = new Array[Int](size)
            val c        = new Array[Double](size)
            val volume   = new Array[Double](size)
            val dev      = new Array[Double](size)
            val perCarat = new Array[Double](size)
            var i        = 0
            while i < size do
                p(i) = price(i) * 2 + 1
                c(i) = carat(i) * 100.0
                volume(i) = x(i) * y(i) * z(i)
                val d = depth(i) - 60.0
                dev(i) = d * d
                perCarat(i) = price(i).toDouble / carat(i)
                i += 1
            bh.consume(ColumnBatch(size, Map("p" -> p, "c" -> c, "volume" -> volume, "dev" -> dev, "perCarat" -> perCarat)))
            rows += size
        rows
