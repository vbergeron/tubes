package tubes.bench.h2o

import java.util.concurrent.TimeUnit
import scala.reflect.ClassTag
import org.openjdk.jmh.annotations.*
import tubes.*
import org.openjdk.jmh.infra.BenchmarkParams
import NamedTuple.{AnyNamedTuple, Concat, From}

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
  * `HashMap` over the right table and the same output rows, both reading case class values and collecting rows into an
  * `IArray`. Each also runs on the batch kernel, reading the tables stored column by column and returning the joined
  * table as column batches, as columnar engines do, both with the code compiled where each query is written and
  * interpreting every column tree. Setup checks the rows of the benchmark being measured.
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

    /** The questions over the tables as case class values, for the row kernel. */
    var onRows: Questions = null

    /** The questions over the tables stored column by column, for the batch kernel; built only for its benchmarks. */
    var onColumns: Questions = null

    /** The plans of the supported questions, over the left table `x` and the right tables. */
    final class Questions(x: Plan[XRow], small: Plan[SmallRow], medium: Plan[MediumRow], big: Plan[BigRow]):
        val q1: Plan[Q1Join] = x.join(small)($left.id1 === $right.sId1)
        val q2: Plan[Q2Join] = x.join(medium)($left.id2 === $right.mId2)
        val q3: Plan[Q3Join] = x.leftJoin(medium)($left.id2 === $right.mId2)
        val q4: Plan[Q4Join] = x.join(medium)($left.id5 === $right.mId5)
        val q5: Plan[Q5Join] = x.join(big)($left.id3 === $right.bId3)

    @Setup
    def setup(params: BenchmarkParams): Unit =
        data = H2OData.Joins(n)
        val (x, small, medium, big) = (from(data.x), from(data.small), from(data.medium), from(data.big))
        onRows = Questions(x, small, medium, big)
        val benchmark = params.getBenchmark.split('.').last
        if benchmark.startsWith("batch") then
            val (x, small, medium, big) = (
                from(Columns.from(data.x)),
                from(Columns.from(data.small)),
                from(Columns.from(data.medium)),
                from(Columns.from(data.big))
            )
            onColumns = Questions(x, small, medium, big)
        // Only the benchmark being measured, against the hand-written rows, so that forks do not run every question.
        val kernel = benchmark.takeWhile(_ != 'Q')
        benchmark.split('Q').last match
            case "1" => check(benchmark, kernel, handWrittenQ1(), onRows.q1, onColumns.q1)
            case "2" => check(benchmark, kernel, handWrittenQ2(), onRows.q2, onColumns.q2)
            case "3" => check(benchmark, kernel, handWrittenQ3(), onRows.q3, onColumns.q3)
            case "4" => check(benchmark, kernel, handWrittenQ4(), onRows.q4, onColumns.q4)
            case "5" => check(benchmark, kernel, handWrittenQ5(), onRows.q5, onColumns.q5)

    /** Checks that `kernel` gives the `expected` rows, reading `rows` or, for the batch kernel, `columns`. */
    private def check[R <: AnyNamedTuple](name: String, kernel: String, expected: IArray[R], rows: Plan[R], columns: => Plan[R])
        : Unit =
        require(expected.nonEmpty, s"$name: no rows")
        val actual = kernel match
            case "rowKernel"        => rows.materialize(RowKernel(List))
            case "batchKernel"      => columns.materialize(BatchKernel(List))
            case "batchInterpreted" => columns.materialize(BatchKernel.interpreted(List))
            case _                  => expected.toList
        require(expected.sameElements(actual), s"$name: kernel disagrees on ${expected.size} rows")

    /** The right rows by key; db-benchmark right tables hold each key once. */
    private def index[K, R](rows: Vector[R])(key: R => K): java.util.HashMap[K, R] =
        val table = java.util.HashMap[K, R](rows.size * 2)
        rows.foreach(r => table.put(key(r), r))
        table

    /** Joins each left row with the right row of its key, dropping the rows without one, into `row(left, right)`. */
    private inline def innerJoin[K, R, O: ClassTag](table: java.util.HashMap[K, R])(inline key: JoinX => K)(
        inline row: (JoinX, R) => O
    ): IArray[O] =
        val out = Array.newBuilder[O]
        for l <- data.x do
            val r = table.get(key(l))
            if r != null then out += row(l, r)
        IArray.unsafeFromArray(out.result())

    // q1: small inner join on an int key

    @Benchmark def rowKernelQ1(): IArray[Q1Join] = onRows.q1.materialize(RowKernel(IArray))

    @Benchmark def batchKernelQ1(): List[ColumnBatch] = BatchKernel.batches(onColumns.q1).toList

    @Benchmark def batchInterpretedQ1(): List[ColumnBatch] =
        BatchKernel.batches(onColumns.q1, BatchKernel.DefaultBatchSize, compiled = false).toList

    @Benchmark def handWrittenQ1(): IArray[Q1Join] =
        innerJoin(index(data.small)(_.sId1))(_.id1): (l, r) =>
            (
                id1 = l.id1, id2 = l.id2, id3 = l.id3, id4 = l.id4, id5 = l.id5, id6 = l.id6, v1 = l.v1,
                sId1 = r.sId1, sId4 = r.sId4, v2 = r.v2
            )

    // q2: medium inner join on an int key

    @Benchmark def rowKernelQ2(): IArray[Q2Join] = onRows.q2.materialize(RowKernel(IArray))

    @Benchmark def batchKernelQ2(): List[ColumnBatch] = BatchKernel.batches(onColumns.q2).toList

    @Benchmark def batchInterpretedQ2(): List[ColumnBatch] =
        BatchKernel.batches(onColumns.q2, BatchKernel.DefaultBatchSize, compiled = false).toList

    @Benchmark def handWrittenQ2(): IArray[Q2Join] =
        innerJoin(index(data.medium)(_.mId2))(_.id2)(mediumRow)

    // q3: medium left outer join on an int key

    @Benchmark def rowKernelQ3(): IArray[Q3Join] = onRows.q3.materialize(RowKernel(IArray))

    @Benchmark def batchKernelQ3(): List[ColumnBatch] = BatchKernel.batches(onColumns.q3).toList

    @Benchmark def batchInterpretedQ3(): List[ColumnBatch] =
        BatchKernel.batches(onColumns.q3, BatchKernel.DefaultBatchSize, compiled = false).toList

    @Benchmark def handWrittenQ3(): IArray[Q3Join] =
        val table = index(data.medium)(_.mId2)
        val out   = Array.newBuilder[Q3Join]
        for l <- data.x do
            val r = Option(table.get(l.id2))
            val row: Q3Join = (
                id1 = l.id1, id2 = l.id2, id3 = l.id3, id4 = l.id4, id5 = l.id5, id6 = l.id6, v1 = l.v1,
                mId1 = r.map(_.mId1), mId2 = r.map(_.mId2), mId4 = r.map(_.mId4), mId5 = r.map(_.mId5), v2 = r.map(_.v2)
            )
            out += row
        IArray.unsafeFromArray(out.result())

    // q4: medium inner join on a string key

    @Benchmark def rowKernelQ4(): IArray[Q4Join] = onRows.q4.materialize(RowKernel(IArray))

    @Benchmark def batchKernelQ4(): List[ColumnBatch] = BatchKernel.batches(onColumns.q4).toList

    @Benchmark def batchInterpretedQ4(): List[ColumnBatch] =
        BatchKernel.batches(onColumns.q4, BatchKernel.DefaultBatchSize, compiled = false).toList

    @Benchmark def handWrittenQ4(): IArray[Q4Join] =
        innerJoin(index(data.medium)(_.mId5))(_.id5)(mediumRow)

    // q5: big inner join on an int key

    @Benchmark def rowKernelQ5(): IArray[Q5Join] = onRows.q5.materialize(RowKernel(IArray))

    @Benchmark def batchKernelQ5(): List[ColumnBatch] = BatchKernel.batches(onColumns.q5).toList

    @Benchmark def batchInterpretedQ5(): List[ColumnBatch] =
        BatchKernel.batches(onColumns.q5, BatchKernel.DefaultBatchSize, compiled = false).toList

    @Benchmark def handWrittenQ5(): IArray[Q5Join] =
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
