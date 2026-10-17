package tubes.bench.h2o

import java.util.SplittableRandom
import org.openjdk.jmh.infra.Blackhole

/** Reads `rows` one by one into `bh` and counts them. The list is passed by name and only `rest` points into it, so the
  * rows already read can be collected while the rest are computed.
  */
def drain[A](rows: => LazyList[A], bh: Blackhole): Int =
    var rest = rows
    var n    = 0
    while rest.nonEmpty do
        bh.consume(rest.head)
        rest = rest.tail
        n += 1
    n

/** A row of the H2O.ai db-benchmark group-by table. `id1`, `id2` and `id4`, `id5` have 100 distinct values (large
  * groups), `id3` and `id6` have `n / 100` (small groups); `v1` is in 1..5, `v2` in 1..15, `v3` in [0, 100).
  */
case class GroupRow(id1: String, id2: String, id3: String, id4: Int, id5: Int, id6: Int, v1: Int, v2: Int, v3: Double)

/** The left table of the join questions: `id1`, `id2`, `id3` are keys into the small, medium and big right tables,
  * `id4`, `id5`, `id6` the same keys as strings.
  */
case class JoinX(id1: Int, id2: Int, id3: Int, id4: String, id5: String, id6: String, v1: Double)

/** Right tables of the join questions, `n / 1e6`, `n / 1e3` and `n` rows. Their fields are prefixed so that they do
  * not clash with the left table's, as tubes joins require.
  */
case class JoinSmall(sId1: Int, sId4: String, v2: Double)
case class JoinMedium(mId1: Int, mId2: Int, mId4: String, mId5: String, v2: Double)
case class JoinBig(bId1: Int, bId2: Int, bId3: Int, bId4: String, bId5: String, bId6: String, v2: Double)

/** Seeded ports of the db-benchmark data generators (`groupby-datagen.R`, `join-datagen.R`), without missing values
  * and unsorted.
  */
object H2OData:
    val K = 100

    /** The group-by table with `n` rows, in the layout of `G1_<n>_1e2_0_0`. */
    def groupBy(n: Int, seed: Long = 42): Vector[GroupRow] =
        val rnd   = SplittableRandom(seed)
        val small = math.max(1, n / K)
        val large = Vector.tabulate(K)(i => f"id${i + 1}%03d")
        val ids   = Vector.tabulate(small)(i => f"id${i + 1}%010d")
        Vector.fill(n):
            GroupRow(
                id1 = large(rnd.nextInt(K)),
                id2 = large(rnd.nextInt(K)),
                id3 = ids(rnd.nextInt(small)),
                id4 = rnd.nextInt(K) + 1,
                id5 = rnd.nextInt(K) + 1,
                id6 = rnd.nextInt(small) + 1,
                v1 = rnd.nextInt(5) + 1,
                v2 = rnd.nextInt(15) + 1,
                v3 = math.round(rnd.nextDouble() * 100 * 1e6) / 1e6
            )

    /** The join tables for `n` left rows. Each right table holds its keys once; the left keys are drawn from a range
      * about 10% wider, so roughly 90% of the left rows find a match, as in db-benchmark.
      */
    final class Joins(n: Int, seed: Long = 42):
        private val rnd = SplittableRandom(seed)

        val smallSize: Int  = math.max(1, n / 1000000)
        val mediumSize: Int = math.max(1, n / 1000)
        val bigSize: Int    = n

        private def key(size: Int): Int = rnd.nextInt(math.max(size + 1, (size * 1.1).toInt)) + 1

        val x: Vector[JoinX] = Vector.fill(n):
            val (k1, k2, k3) = (key(smallSize), key(mediumSize), key(bigSize))
            JoinX(k1, k2, k3, s"id$k1", s"id$k2", s"id$k3", math.round(rnd.nextDouble() * 100 * 1e6) / 1e6)

        val small: Vector[JoinSmall] = Vector.tabulate(smallSize)(i => JoinSmall(i + 1, s"id${i + 1}", value()))

        val medium: Vector[JoinMedium] = Vector.tabulate(mediumSize): i =>
            val k1 = rnd.nextInt(smallSize) + 1
            JoinMedium(k1, i + 1, s"id$k1", s"id${i + 1}", value())

        val big: Vector[JoinBig] = Vector.tabulate(bigSize): i =>
            val (k1, k2) = (rnd.nextInt(smallSize) + 1, rnd.nextInt(mediumSize) + 1)
            JoinBig(k1, k2, i + 1, s"id$k1", s"id$k2", s"id${i + 1}", value())

        private def value(): Double = math.round(rnd.nextDouble() * 100 * 1e6) / 1e6
