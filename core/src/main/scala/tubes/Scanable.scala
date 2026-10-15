package tubes

import NamedTuple.{AnyNamedTuple, From}

/** A value that can be read as rows described by `schema(source)`.
  *
  * `scan` yields rows in the source's own representation (e.g. case class instances): any `Product` whose elements are
  * laid out like `Row`. Kernels read them positionally, without converting them first.
  */
trait Scanable[S]:
    type Row <: AnyNamedTuple
    def schema(source: S): Schema[Row]
    def scan(source: S): Iterator[Product]

object Scanable:
    type Aux[S, R <: AnyNamedTuple] = Scanable[S] { type Row = R }

    def make[S, R <: AnyNamedTuple](f: S => Schema[R], g: S => Iterator[Product]): Aux[S, R] =
        new Scanable[S]:
            type Row = R
            def schema(source: S) = f(source)
            def scan(source: S)   = g(source)

    /** A sequence of case class values, one row per element, scanned as the values themselves. */
    inline given seq[C[x] <: Seq[x], A <: Product]: Aux[C[A], From[A]] =
        val s = Schema.derived[A]
        make(_ => s, _.iterator)

/** A value packed with the `Scanable` that reads it, so it can be scanned without knowing its type. */
sealed trait Source[Row <: AnyNamedTuple]:
    type S
    def value: S
    def scanable: Scanable.Aux[S, Row]

    def schema: Schema[Row]     = scanable.schema(value)
    def scan: Iterator[Product] = scanable.scan(value)

    override def toString: String = s"Source($value)"

object Source:
    def apply[S0, Row <: AnyNamedTuple](v: S0, s: Scanable.Aux[S0, Row]): Source[Row] =
        new Source[Row]:
            type S = S0
            val value    = v
            val scanable = s
