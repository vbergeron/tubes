package tubes.bench

import java.net.URI
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*

case class Diamond(
    carat: Double,
    cut: String,
    color: String,
    clarity: String,
    depth: Double,
    table: Double,
    price: Int,
    x: Double,
    y: Double,
    z: Double
)

/** The seaborn `diamonds` dataset (~54k rows), cached under `benchmarks/data/`. */
object Diamonds:
    val url = "https://raw.githubusercontent.com/mwaskom/seaborn-data/master/diamonds.csv"

    /** The cached CSV, downloaded on first use; resolved from either the repo root or `benchmarks/`. */
    def file: Path =
        val path = Seq(Paths.get("benchmarks", "data", "diamonds.csv"), Paths.get("data", "diamonds.csv"))
            .find(p => Files.exists(p))
            .getOrElse(Paths.get("benchmarks", "data", "diamonds.csv"))
        if !Files.exists(path) then
            Files.createDirectories(path.getParent)
            val in = URI.create(url).toURL.openStream()
            try Files.copy(in, path) finally in.close()
        path

    def load(): Vector[Diamond] =
        Files.readAllLines(file).asScala.iterator.drop(1).map(parse).toVector

    private def parse(line: String): Diamond =
        val f = line.split(',').map(_.stripPrefix("\"").stripSuffix("\""))
        Diamond(
            f(0).toDouble, f(1), f(2), f(3), f(4).toDouble, f(5).toDouble,
            f(6).toInt, f(7).toDouble, f(8).toDouble, f(9).toDouble
        )
