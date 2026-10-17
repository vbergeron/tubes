package tubes.bench.h2o

import java.lang.management.ManagementFactory
import scala.jdk.CollectionConverters.*
import org.openjdk.jmh.infra.Blackhole

/** A quick check of H2O join q5 outside JMH, in about three minutes where `H2OJoinBench` takes a fork per variant.
  * Generates the tables once, then runs each variant `runs` times in this JVM, the first run as warmup, and prints its
  * time, allocations and GC time. These are single runs: read differences under 15% as noise.
  *
  * sbt "benchmarks/runMain tubes.bench.h2o.quickJoinQ5 [n] [runs]"
  */
@main def quickJoinQ5(args: String*): Unit =
    val n    = args.headOption.fold(10000000)(_.toInt)
    val runs = args.lift(1).fold(2)(_.toInt)
    val bh   = Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.")

    val bench = H2OJoinBench()
    bench.n = n
    val generated = timed(bench.prepare())
    println(f"n = $n%,d, tables generated in ${generated / 1e9}%.1f s")

    val variants = List[(String, () => Int)](
        "tubes, IArray"   -> (() => bench.rowKernelQ5().length),
        "tubes, Iterator" -> (() => bench.rowKernelStreamQ5(bh)),
        "hand, IArray"    -> (() => bench.handWrittenQ5().length),
        "hand, Iterator"  -> (() => bench.handWrittenStreamQ5(bh))
    )
    println(f"${"variant"}%-16s ${"run"}%4s ${"rows"}%11s ${"time s"}%7s ${"alloc GB"}%9s ${"GC s"}%6s")
    for (name, run) <- variants; i <- 1 to runs do
        System.gc()
        val (gcBefore, allocBefore) = (gcMillis, allocated)
        var rows  = 0
        val nanos = timed { rows = run() }
        val label = if i == 1 && runs > 1 then "warm" else i.toString
        println(
            f"$name%-16s $label%4s $rows%,11d ${nanos / 1e9}%7.2f ${(allocated - allocBefore) / 1e9}%9.2f ${(gcMillis - gcBefore) / 1e3}%6.2f"
        )

private def timed(body: => Unit): Long =
    val start = System.nanoTime()
    body
    System.nanoTime() - start

/** Bytes allocated so far by this thread. */
private def allocated: Long =
    ManagementFactory.getThreadMXBean.asInstanceOf[com.sun.management.ThreadMXBean].getCurrentThreadAllocatedBytes

/** Milliseconds spent in GC so far, all collectors together. */
private def gcMillis: Long = ManagementFactory.getGarbageCollectorMXBeans.asScala.map(_.getCollectionTime).sum
