package tubes

import NamedTuple.AnyNamedTuple

/** Group-bys and joins compiled for the batch kernel must compute what the interpreter computes, in arrays of the same
  * classes, and what the row kernel computes.
  */
class BatchGroupJoinCompilerSuite extends munit.FunSuite:
    case class Money(cents: Int)
    given Ordering[Money] = Ordering.by[Money, Int](_.cents).reverse
    given Numeric[Money] with
        def plus(x: Money, y: Money)    = Money(x.cents + y.cents)
        def minus(x: Money, y: Money)   = Money(x.cents - y.cents)
        def times(x: Money, y: Money)   = Money(x.cents * y.cents)
        def negate(x: Money)            = Money(-x.cents)
        def fromInt(x: Int)             = Money(x)
        def parseString(str: String)    = str.toIntOption.map(Money(_))
        def toInt(x: Money)             = x.cents
        def toLong(x: Money)            = x.cents.toLong
        def toFloat(x: Money)           = x.cents.toFloat
        def toDouble(x: Money)          = x.cents.toDouble
        def compare(x: Money, y: Money) = summon[Ordering[Money]].compare(x, y)

    case class Sale(
        shop: String,
        item: String,
        qty: Int,
        units: Long,
        price: Double,
        open: Boolean,
        tag: Option[Int],
        cash: Money
    )
    case class Shop(shopName: String, city: String, zone: Int)
    case class Item(itemName: String, shopRef: String, cost: Double)

    val sales = List(
        Sale("north", "apple", 3, 30L, 1.5, true, Some(1), Money(4)),
        Sale("south", "pear", 1, 10L, Double.NaN, false, None, Money(2)),
        Sale("north", "pear", 2, 20L, -0.0, true, Some(1), Money(5)),
        Sale("north", "apple", 5, 50L, 0.0, true, Some(2), Money(5)),
        Sale("south", "apple", 4, 40L, 1.25, false, None, Money(7)),
        Sale("east", "fig", 7, 70L, Double.NaN, true, Some(1), Money(1)),
        Sale("north", "apple", 1, 10L, 2.0, true, Some(1), Money(3))
    )
    val shops = List(Shop("north", "Lyon", 1), Shop("south", "Nice", 2), Shop("west", "Brest", 1), Shop("north", "Lille", 3))
    val items = List(Item("apple", "north", 1.0), Item("pear", "south", 2.0), Item("apple", "south", 0.5), Item("kiwi", "east", 3.0))

    /** Every batch of `plan`, compiled and interpreted, must hold the same values in arrays of the same classes; the
      * batch kernel must give the row kernel's rows, when `rows`.
      */
    def same[Out <: AnyNamedTuple](plan: Plan[Out], rows: Boolean = true)(using munit.Location): Unit =
        for size <- List(1, 2, 3, BatchKernel.DefaultBatchSize) do
            val compiled    = BatchKernel.batches(plan, size, compiled = true).toList
            val interpreted = BatchKernel.batches(plan, size, compiled = false).toList
            assertEquals(compiled.map(_.size), interpreted.map(_.size), s"batch size $size")
            for (c, i) <- compiled.zip(interpreted); name <- plan.schema.names do
                val (a, b) = (c(name), i(name))
                assertEquals(a.getClass, b.getClass, s"column $name, batch size $size")
                // Compared as strings, so that NaN equals NaN.
                assertEquals(a.toList.map(_.toString), b.toList.map(_.toString), s"column $name, batch size $size")
        if rows then assertEquals(plan.materialize(BatchKernel(List)).toString, plan.materialize(RowKernel(List)).toString)

    test("group-bys compile their keys and aggregations at their call site"):
        val plan = from(sales).groupBy(($.shop, $.item))($.qty.sum.as("qty")).asInstanceOf[GroupBySpec[?]]
        assert(plan.batchKeys.columns.forall(_.isDefined))
        assert(plan.batchKeys.codec.isDefined)
        assert(plan.fold.isDefined)
        val byInt = from(sales).groupBy($.qty)(count.as("n")).asInstanceOf[GroupBySpec[?]]
        assert(byInt.batchKeys.codec.isEmpty, "a single Int key keeps the kernel's own maps")
        assert(byInt.fold.isDefined)

    test("primitive aggregations over keys of every kind"):
        same(from(sales).groupBy($.shop)((
            $.qty.sum.as("q"),
            $.units.sum.as("u"),
            $.price.sum.as("p"),
            $.qty.avg.as("aq"),
            $.units.avg.as("au"),
            $.price.avg.as("ap"),
            count.as("n")
        )))
        same(from(sales).groupBy(($.shop, $.item))(($.qty.min.as("lo"), $.qty.max.as("hi"), $.units.max.as("u"))))
        same(from(sales).groupBy(($.open, $.tag, $.units))(($.price.min.as("lo"), $.price.max.as("hi"))))
        same(from(sales).groupBy(($.shop, $.item, $.qty, $.units, $.open, $.tag))(count.as("n")))

    test("Double keys, NaN and signed zeros group as the interpreter groups them"):
        // The row kernel groups a single key by its boxed value's `equals`, which puts NaNs together and -0.0 apart
        // from 0.0; the batch kernel compares keys with `==` either way.
        same(from(sales).groupBy($.price)(count.as("n")), rows = false)
        same(from(sales).groupBy(($.price, $.shop))($.qty.sum.as("q")))

    test("aggregations of expressions, boxed values and other instances"):
        same(from(sales).groupBy($.shop)((($.qty * 2 + 1).sum.as("s"), ($.price * $.qty.toDouble).avg.as("w"))))
        same(from(sales).groupBy($.shop)(($.item.min.as("first"), $.item.max.as("last"), $.tag.default(0).sum.as("t"))))
        same(from(sales).groupBy($.item)(($.cash.sum.as("cash"), $.cash.min.as("lo"), $.cash.max.as("hi"), $.cash.avg.as("avg"))))

    test("outputs computed from aggregations, and repeated aggregations"):
        same(from(sales).groupBy($.shop)((($.qty.max - $.qty.min).as("range"), $.qty.max.as("hi"))))
        same(from(sales).groupBy($.item)((($.units.sum.toDouble / count.toDouble).as("mean"), $.units.sum.as("u"))))
        same(from(sales).filter($.qty > 1).groupBy(($.shop, $.item))((($.price.max + 1.0) * 2.0).as("x")))

    test("many groups, growing the tables and the running values"):
        case class Event(user: String, day: Int, amount: Long)
        val events = List.tabulate(20000)(i => Event(s"u${i % 3001}", i % 7, i.toLong))
        same(from(events).groupBy(($.user, $.day))(($.amount.sum.as("total"), count.as("n"), $.amount.max.as("top"))))
        same(from(events).groupBy($.user)($.day.avg.as("d")))

    test("an empty input has no groups"):
        same(from(List.empty[Sale]).groupBy(($.shop, $.item))($.qty.sum.as("q")))

    test("joins compile their keys and residual at their call site"):
        val plan = from(sales).join(from(shops))($left.shop === $right.shopName).asInstanceOf[JoinSpec[?]]
        assert(plan.batch.exists(b => b.codec.isDefined && b.residual.isEmpty))
        val byInt = from(sales).join(from(shops))(($left.qty === $right.zone) && ($right.city =!= "Nice"))
        byInt match
            case j: JoinSpec[?] =>
                assert(j.batch.exists(b => b.codec.isEmpty && b.residual.isDefined && b.residualFn.isDefined))
            case other => fail(s"expected a join, got $other")

    test("joins on string, composite, expression and Double keys, with residuals"):
        same(from(sales).join(from(shops))($left.shop === $right.shopName))
        same(from(sales).join(from(items))(($left.item === $right.itemName) && ($left.shop === $right.shopRef)))
        same(from(sales).join(from(shops))(($left.qty - 1 === $right.zone) && ($right.city =!= "Lille")))
        same(from(sales).join(from(items))(($left.price === $right.cost) && ($left.qty > 1)))

    test("outer joins keep their unmatched rows"):
        same(from(sales).leftJoin(from(shops))($left.shop === $right.shopName))
        same(from(sales).rightJoin(from(items))(($left.item === $right.itemName) && ($left.shop === $right.shopRef)))
        same(from(sales).fullJoin(from(shops))(($left.shop === $right.shopName) && ($right.zone > 1)))

    test("compiled joins and group-bys compose"):
        same(
            from(sales)
                .join(from(shops))($left.shop === $right.shopName)
                .groupBy(($.city, $.item))(($.qty.sum.as("q"), ($.price.max - $.price.min).as("spread")))
        )
