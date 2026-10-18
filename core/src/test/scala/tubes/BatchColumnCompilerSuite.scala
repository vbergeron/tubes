package tubes

import NamedTuple.AnyNamedTuple

/** Filters and selects compiled for the batch kernel must compute what the interpreter computes, in arrays of the same
  * classes.
  */
class BatchColumnCompilerSuite extends munit.FunSuite:
    case class Reading(id: Int, ticks: Long, value: Double, ok: Boolean, label: String, weight: Option[Int])
    case class Money(cents: Int)

    given Ordering[Money] = Ordering.by[Money, Int](_.cents).reverse
    given Numeric[Money] with
        def plus(x: Money, y: Money)  = Money(x.cents + y.cents)
        def minus(x: Money, y: Money) = Money(x.cents - y.cents)
        def times(x: Money, y: Money) = Money(x.cents * y.cents)
        def negate(x: Money)          = Money(-x.cents)
        def fromInt(x: Int)           = Money(x)
        def parseString(str: String)  = str.toIntOption.map(Money(_))
        def toInt(x: Money)           = x.cents
        def toLong(x: Money)          = x.cents.toLong
        def toFloat(x: Money)         = x.cents.toFloat
        def toDouble(x: Money)        = x.cents.toDouble
        def compare(x: Money, y: Money) = summon[Ordering[Money]].compare(x, y)

    case class Purse(owner: String, cash: Money)

    val readings = List(
        Reading(1, 10L, 1.5, true, "a", Some(3)),
        Reading(2, -4L, Double.NaN, false, "b", None),
        Reading(3, 7L, -0.0, true, "a", Some(0)),
        Reading(4, 0L, 2.25, false, "c", Some(9)),
        Reading(5, 99L, 0.0, true, "b", None)
    )
    val purses = List(Purse("ann", Money(5)), Purse("ben", Money(12)), Purse("cat", Money(7)))

    /** Every batch of `plan`, compiled and interpreted, must hold the same values in arrays of the same classes. */
    def same[Out <: AnyNamedTuple](plan: Plan[Out])(using munit.Location): Unit =
        for size <- List(1, 2, BatchKernel.DefaultBatchSize) do
            val compiled    = BatchKernel.batches(plan, size, compiled = true).toList
            val interpreted = BatchKernel.batches(plan, size, compiled = false).toList
            assertEquals(compiled.map(_.size), interpreted.map(_.size), s"batch size $size")
            for (c, i) <- compiled.zip(interpreted); name <- plan.schema.names do
                val (a, b) = (c(name), i(name))
                assertEquals(a.getClass, b.getClass, s"column $name, batch size $size")
                // Compared as strings, so that NaN equals NaN.
                assertEquals(a.toList.map(_.toString), b.toList.map(_.toString), s"column $name, batch size $size")
        assertEquals(plan.materialize(BatchKernel(List)).toString, plan.materialize(RowKernel(List)).toString)

    test("filters and selects compile at their call site"):
        val plan = from(readings).filter($.id > 1).select(($.label, ($.value * 2.0).as("twice")))
        plan match
            case s: SelectSpec[?] =>
                assert(s.batch.forall(_.isDefined))
                s.input match
                    case f: FilterSpec[?] => assert(f.batch.isDefined)
                    case other            => fail(s"expected a filter, got $other")
            case other => fail(s"expected a select, got $other")

    test("primitive arithmetic, comparisons and division write primitive arrays"):
        same(
            from(readings).select((
                ($.id * 3 - 1).as("i"),
                ($.ticks + 1L).as("l"),
                ($.value * 2.0 + 0.5).as("d"),
                ($.id / 2).as("q"),
                ($.ticks / 3L).as("lq"),
                ($.value / 2.0).as("half"),
                $.id.toDouble.as("di"),
                ($.ticks.toDouble / 2.0).as("dl"),
                ($.id < 3).as("lt"),
                ($.ticks >= 7L).as("ge"),
                ($.id === 4).as("eq")
            ))
        )

    test("Double comparisons keep the standard ordering, which orders NaN"):
        same(from(readings).select((($.value < 1.0).as("lt"), ($.value >= 0.0).as("ge"), ($.value === 0.0).as("eq"))))
        same(from(readings).filter($.value > 0.0))

    test("an ordering given locally, such as the IEEE one on Double, is called as it is"):
        given Ordering[Double] = Ordering.Double.IeeeOrdering
        same(from(readings).select((($.value < 1.0).as("lt"), ($.value >= 0.0).as("ge"), ($.value <= -0.0).as("le"))))

    test("logic, literals and boxed fields"):
        same(from(readings).filter(($.ok && $.label =!= "c") || !($.id <= 4)).select(($.id, $.label)))
        same(from(readings).select((Column.lit(7).as("seven"), Column.lit("x").as("x"), ($.label === "a").as("isA"))))

    test("the right side of && and || is only computed where the left side does not decide"):
        same(from(readings).filter(($.ticks =!= 0L) && (Column.lit(100L) / $.ticks > 1L)))
        same(from(readings).filter(($.ticks === 0L) || (Column.lit(100L) / $.ticks < 50L)))

    test("optional fields and defaults"):
        same(from(readings).select(($.weight, $.weight.default(-1).as("w"), ($.weight.default(0) * 2).as("w2"))))
        same(from(readings).filter($.weight.default(0) > 2))

    test("instances other than the standard ones are called as they are"):
        same(from(purses).filter($.cash > Money(6)).select(($.owner, ($.cash + Money(1)).as("more"))))
        same(from(purses).select(Tuple1($.cash.toDouble.as("d"))))

    test("compiled columns read batches whose arrays are held otherwise, such as boxed ones"):
        val batch = ColumnBatch(2, Map("id" -> Array[AnyRef](Int.box(4), Int.box(5)), "ticks" -> Array(1L, 2L)))
        val plan  = from[(id: Int, ticks: Long)].select(Tuple1(($.id + 1).as("next")))
        val fn    = plan.asInstanceOf[SelectSpec[?]].batch.head.get
        assertEquals(fn(batch).toList: List[Any], List(5, 6))
