# Benchmarks

JMH benchmarks for tubes. Every benchmark runs the same query twice: through tubes' row kernel, and as the loop
a Scala developer would write by hand for that one query. The hand-written version is the bar to meet: tubes
compiles queries at their call site, so on a single thread the goal is to run as fast as that hand-written code.

## Suites

| Suite | What it measures |
|---|---|
| `ColumnBench` | A single compiled column expression applied to rows |
| `RowKernelBench` | A filter / select pipeline over the diamonds dataset |
| `JoinBench` | Hash joins: small build side, composite keys, large build side |
| `GroupByBench` | Group-by with aggregates over the diamonds dataset |
| `h2o.H2OGroupByBench` | The group-by questions of the [H2O.ai db-benchmark](https://h2oai.github.io/db-benchmark/) |
| `h2o.H2OJoinBench` | The join questions of the same benchmark |

`RowKernelBench` and `GroupByBench` read `benchmarks/data/diamonds.csv`, which is not checked in. The H2O suites
generate their data with seeded ports of the db-benchmark generators (`H2OData`). Their setup checks that tubes and
the hand-written version return the same rows.

## Running

```sh
# everything, with the default sizes
sbt "benchmarks/Jmh/run"

# the H2O suites at db-benchmark's smallest size (1e7 rows), with a heap large enough to hold it
sbt "benchmarks/Jmh/run -p n=10000000 -f 1 -wi 2 -i 3 -jvmArgs -Xmx10g -jvmArgs -Xms10g -prof gc tubes.bench.h2o"
```

After renaming a benchmark or changing its return type, run `sbt benchmarks/clean`: otherwise JMH can run its stale
generated code and fail with `NoSuchMethodError`.

## H2O db-benchmark results at 1e7 rows

### Setup

- **tubes and hand-written:** Intel Core i7-14700HX, Windows 11, Temurin JDK 21.0.12, `-Xmx10g -Xms10g`, a single
  thread. JMH ran 1 fork, 2 warmup and 3 measured iterations, with results collected into an `IArray` on both sides.
- **Other engines:** the latest published runs from the
  [duckdblabs db-benchmark](https://duckdblabs.github.io/db-benchmark/) (the maintained continuation of H2O's) on
  their `c6id.4xlarge` machine (16 vCPU), on the same tables (`G1_1e7_1e2_0_0`, `J1_1e7_NA_0_0`), best of their two
  runs. They come from the repository's `time.csv` and were fetched in October 2026.

All times are in seconds.

### Group-by

| Question | **tubes** | hand-written | duckdb | polars | datafusion | data.table | spark |
|---|---:|---:|---:|---:|---:|---:|---:|
| q1 sum v1 by id1 | **0.19** | 0.11 | 0.005 | 0.041 | 0.019 | 0.081 | 0.61 |
| q2 sum v1 by id1:id2 | **0.46** | 0.43 | 0.037 | 0.084 | 0.042 | 0.079 | 0.77 |
| q3 sum v1, mean v3 by id3 | **1.50** | 0.51 | 0.127 | 0.081 | 0.112 | 0.107 | 1.13 |
| q4 mean v1:v3 by id4 | **0.20** | 0.12 | 0.011 | 0.017 | 0.019 | 0.181 | 0.51 |
| q5 sum v1:v3 by id6 | **1.65** | 0.44 | 0.124 | 0.089 | 0.072 | 0.114 | 0.74 |
| q10 sum v3, count by id1:id6 | **6.2** | 6.0 | 0.30 | 0.63 | 0.37 | 0.93 | 4.3 |

q7 (max v1 - min v2 by id3) runs in `H2OGroupByBench` now that aggregations combine with column arithmetic; it has
not been measured against the other engines yet. Questions q6, q8 and q9 need features tubes does not have yet: median
and standard deviation, top-n per group, and correlation. `H2OGroupByBench` keeps them as placeholders, with the syntax
they would use.

### Joins

| Question | **tubes** | hand-written | duckdb | polars | datafusion | data.table | spark |
|---|---:|---:|---:|---:|---:|---:|---:|
| q1 small inner on int | **1.9** | 1.3 | 0.14 | 0.075 | 0.091 | 0.17 | 2.7 |
| q2 medium inner on int | **2.5** | 1.4 | 0.16 | 0.078 | 0.12 | 0.22 | 3.2 |
| q3 medium outer on int | **3.4** | 2.1 | 0.20 | 0.078 | 0.14 | 0.40 | 3.5 |
| q4 medium inner on string | **2.0** | 2.2 | 0.17 | 0.079 | 0.18 | 0.22 | 3.2 |
| q5 big inner on int | **8.7** | 5.2 | 0.44 | 0.43 | 0.51 | 1.4 | 7.2 |

### Reading the numbers

**Against the other engines,** tubes lands next to Spark, and 10 to 40 times behind DuckDB, Polars and DataFusion.
That gap is not a like-for-like measurement:

- **Threads:** the other engines use all 16 cores; the row kernel uses one thread.
- **Layout:** the other engines are columnar. tubes holds rows as JVM objects, so a join emitting 10M rows allocates
  10M row objects: 1.4 to 2.8 GB per run, of which the GC takes 1 to 3 seconds.
- **Hardware:** a laptop is not their AWS instance.

Hand-written Scala hits the same walls: it is just as far behind the columnar engines. Closing that gap takes a
different kernel (columnar, parallel), not a faster row kernel. Plans are independent of the kernel that compiles
them, so that kernel can be added next to the row kernel.

**Against hand-written code,** tubes is at parity on group-by q2 and q10 and on join q4. It is 1.5 to 1.7 times
slower on joins q1 to q3 and on group-by q1 and q4, about as at 1e6 rows: every row is read through generic
`productElement` calls and a key function, where the hand-written loop reads typed fields. Two gaps grow at this size:

- **Group-by q3 and q5, about 3 times slower (2 times at 1e6).** These are the questions with 100k groups, and
  neither side triggers a GC, so the cost is CPU. The likely cause is cache misses: each tubes group holds an array of
  separate accumulator objects, where the hand-written loop keeps one flat object per group.
- **Join q5, about 1.7 times slower.** tubes allocates less than the hand-written join and spends half as long in GC
  (0.95 s against 1.9 s), so the cost is in its hash table over 10M right rows. A lookup reads up to four separate
  arrays and then the row, where `java.util.HashMap` follows a single node.

### Caveats

Each measurement is 3 iterations, often one query per iteration, so JMH's error bars are wide (up to ±300% on
joins). Most benchmarks' iterations agree within 30%, but a few spread further: up to 1.5 times for group-by q5 and
1.6 times for the hand-written join q4. Read ratios under 1.3 as parity (join q4 included), and the larger gaps as
real; the tenths of a second are not significant.
The result collection makes no visible difference at this size: an `IArray` saves 16 bytes per row over a `List`,
which is small next to the rows themselves.
