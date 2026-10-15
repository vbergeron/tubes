package tubes.bench.h2o

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.*
import tubes.*
import NamedTuple.{Concat, From}

type XRow      = From[JoinX]
type SmallRow  = From[JoinSmall]
type MediumRow = From[JoinMedium]
type BigRow    = From[JoinBig]

type Q1Join = Concat[XRow, SmallRow]
type Q2Join = Concat[XRow, MediumRow]
type Q3Join = Concat[XRow, Optional[MediumRow]]
type Q4Join = Concat[XRow, MediumRow]
type Q5Join = Concat[XRow, BigRow]

/** The join questions of the H2O.ai db-benchmark (https://h2oai.github.io/db-benchmark/), joining `n` left rows with
  * right tables of `n / 1e6`, `n / 1e3` and `n` rows. Like db-benchmark, each question returns the full joined table:
  * the left fields followed by the right ones. Each runs on the row kernel and as a hand-written hash join building a
  * `HashMap` over the right table and the same output rows; setup checks both give the same rows.
  */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 2)
@Measurement(iterations = 5, time = 2)
@Fork(2)
class H2OJoinBench:

    @Param(Array("1000000"))
    var n: Int = 0

    var data: H2OData.Joins = null

    var q1: Plan[Q1Join] = null
    var q2: Plan[Q2Join] = null
    var q3: Plan[Q3Join] = null
    var q4: Plan[Q4Join] = null
    var q5: Plan[Q5Join] = null

    @Setup
    def setup(): Unit =
        data = H2OData.Joins(n)
        val x = from(data.x)
        q1 = x.join(from(data.small))($left.id1 === $right.sId1)
        q2 = x.join(from(data.medium))($left.id2 === $right.mId2)
        q3 = x.leftJoin(from(data.medium))($left.id2 === $right.mId2)
        q4 = x.join(from(data.medium))($left.id5 === $right.mId5)
        q5 = x.join(from(data.big))($left.id3 === $right.bId3)
        check("q1", handWrittenQ1(), rowKernelQ1())
        check("q2", handWrittenQ2(), rowKernelQ2())
        check("q3", handWrittenQ3(), rowKernelQ3())
        check("q4", handWrittenQ4(), rowKernelQ4())
        check("q5", handWrittenQ5(), rowKernelQ5())

    private def check(name: String, expected: List[?], actual: List[?]): Unit =
        require(expected.nonEmpty, s"$name: no rows")
        require(expected == actual, s"$name: kernel disagrees on ${expected.size} rows")

    /** The right rows by key; db-benchmark right tables hold each key once. */
    private def index[K, R](rows: Vector[R])(key: R => K): java.util.HashMap[K, R] =
        val table = java.util.HashMap[K, R](rows.size * 2)
        rows.foreach(r => table.put(key(r), r))
        table

    /** Joins each left row with the right row of its key, dropping the rows without one, into `row(left, right)`. */
    private inline def innerJoin[K, R, O](table: java.util.HashMap[K, R])(inline key: JoinX => K)(
        inline row: (JoinX, R) => O
    ): List[O] =
        val out = List.newBuilder[O]
        for l <- data.x do
            val r = table.get(key(l))
            if r != null then out += row(l, r)
        out.result()

    // q1: small inner join on an int key

    @Benchmark def rowKernelQ1(): List[Q1Join] = q1.materialize(RowKernel())

    @Benchmark def handWrittenQ1(): List[Q1Join] =
        innerJoin(index(data.small)(_.sId1))(_.id1): (l, r) =>
            (
                id1 = l.id1, id2 = l.id2, id3 = l.id3, id4 = l.id4, id5 = l.id5, id6 = l.id6, v1 = l.v1,
                sId1 = r.sId1, sId4 = r.sId4, v2 = r.v2
            )

    // q2: medium inner join on an int key

    @Benchmark def rowKernelQ2(): List[Q2Join] = q2.materialize(RowKernel())

    @Benchmark def handWrittenQ2(): List[Q2Join] =
        innerJoin(index(data.medium)(_.mId2))(_.id2)(mediumRow)

    // q3: medium left outer join on an int key

    @Benchmark def rowKernelQ3(): List[Q3Join] = q3.materialize(RowKernel())

    @Benchmark def handWrittenQ3(): List[Q3Join] =
        val table = index(data.medium)(_.mId2)
        val out   = List.newBuilder[Q3Join]
        for l <- data.x do
            val r = Option(table.get(l.id2))
            val row: Q3Join = (
                id1 = l.id1, id2 = l.id2, id3 = l.id3, id4 = l.id4, id5 = l.id5, id6 = l.id6, v1 = l.v1,
                mId1 = r.map(_.mId1), mId2 = r.map(_.mId2), mId4 = r.map(_.mId4), mId5 = r.map(_.mId5), v2 = r.map(_.v2)
            )
            out += row
        out.result()

    // q4: medium inner join on a string key

    @Benchmark def rowKernelQ4(): List[Q4Join] = q4.materialize(RowKernel())

    @Benchmark def handWrittenQ4(): List[Q4Join] =
        innerJoin(index(data.medium)(_.mId5))(_.id5)(mediumRow)

    // q5: big inner join on an int key

    @Benchmark def rowKernelQ5(): List[Q5Join] = q5.materialize(RowKernel())

    @Benchmark def handWrittenQ5(): List[Q5Join] =
        innerJoin(index(data.big)(_.bId3))(_.id3): (l, r) =>
            (
                id1 = l.id1, id2 = l.id2, id3 = l.id3, id4 = l.id4, id5 = l.id5, id6 = l.id6, v1 = l.v1,
                bId1 = r.bId1, bId2 = r.bId2, bId3 = r.bId3, bId4 = r.bId4, bId5 = r.bId5, bId6 = r.bId6, v2 = r.v2
            )

    private def mediumRow(l: JoinX, r: JoinMedium): Q2Join =
        (
            id1 = l.id1, id2 = l.id2, id3 = l.id3, id4 = l.id4, id5 = l.id5, id6 = l.id6, v1 = l.v1,
            mId1 = r.mId1, mId2 = r.mId2, mId4 = r.mId4, mId5 = r.mId5, v2 = r.v2
        )
