package tubes

class OptionSuite extends munit.FunSuite:
    case class Person(name: String, age: Option[Int], nickname: Option[String])
    case class Pet(owner: Option[String], pet: String)

    val people = List(Person("alice", Some(30), Some("al")), Person("bob", None, None), Person("carol", Some(15), None))

    test("optional fields are typed `Option[A]` in the schema"):
        val plan = from(people).select(($.name, $.age))
        assertEquals(plan.materialize(RowKernel()).map(_.age), List(Some(30), None, Some(15)))

    test("default replaces `None` with a literal"):
        val plan = from(people).select(($.name, $.age.default(0).as("age")))
        assertEquals(plan.materialize(RowKernel()).map(_.age), List(30, 0, 15))

    test("default replaces `None` with another column"):
        val plan = from(people).select(Tuple1($.nickname.default($.name).as("called")))
        assertEquals(plan.materialize(RowKernel()).map(_.called), List("al", "bob", "carol"))

    test("defaulted columns support the operations of their value type"):
        val plan = from(people).filter($.age.default(18) >= 18).select(Tuple1(($.age.default(18) + 1).as("next")))
        assertEquals(plan.materialize(RowKernel()).map(_.next), List(31, 19))

    test("optional columns compare with options"):
        val plan = from(people).filter($.age === Some(30)).select(Tuple1($.name))
        assertEquals(plan.materialize(RowKernel()).map(_.name), List("alice"))

    test("defaulted columns can be join keys"):
        val pets = List(Pet(Some("alice"), "cat"), Pet(None, "stray"), Pet(Some("bob"), "dog"))
        val plan = from(people)
            .join(from(pets))($left.name === $right.owner.default("bob"))
            .select(($.name, $.pet))
        assertEquals(
            plan.materialize(RowKernel()),
            List((name = "alice", pet = "cat"), (name = "bob", pet = "stray"), (name = "bob", pet = "dog"))
        )

    test("arithmetic on an optional column is optional, and missing where it is"):
        val plan = from(people).select(Tuple1(($.age + 1).as("next")))
        assertEquals(plan.materialize(RowKernel()).map(_.next), List(Some(31), None, Some(16)))

    test("arithmetic mixes optional and plain columns, either way round"):
        case class Row(a: Int, b: Option[Int], c: Option[Int])
        val rows = List(Row(1, Some(2), Some(3)), Row(4, None, Some(5)), Row(6, Some(7), None))
        val plan = from(rows).select((($.a + $.b).as("ab"), ($.b * $.a).as("ba"), ($.b - $.c).as("bc"), ($.a + $.a).as("aa")))
        assertEquals(
            plan.materialize(RowKernel()),
            List(
                (ab = Some(3), ba = Some(2), bc = Some(-1), aa = 2),
                (ab = None, ba = None, bc = None, aa = 8),
                (ab = Some(13), ba = Some(42), bc = None, aa = 12)
            )
        )

    test("comparisons on optional columns are optional booleans, defaulted to filter"):
        val compared = from(people).select(Tuple1(($.age >= 18).as("adult")))
        assertEquals(compared.materialize(RowKernel()).map(_.adult), List(Some(true), None, Some(false)))
        val plan = from(people).filter(($.age >= 18).default(false)).select(Tuple1($.name))
        assertEquals(plan.materialize(RowKernel()).map(_.name), List("alice"))

    test("operators are optional only when a side is"):
        given Schema[NamedTuple.From[Person]] = Schema.derived[Person]
        val plain: Column[Boolean]            = $.age.default(0) + 1 > 1
        val optional: Column[Option[Boolean]] = $.age > $.age.default(0)
        assert(compiletime.testing.typeChecks("""val c: Column[Boolean] = $.age > 1""") == false)

    test("operators reject operands of another value type"):
        given Schema[NamedTuple.From[Person]] = Schema.derived[Person]
        val errors = compiletime.testing.typeCheckErrors("""$.age + 1.5""")
        assert(errors.map(_.message).exists(_.startsWith("Cannot combine Double values with a column of")), errors)
        assert(!compiletime.testing.typeChecks("""$.age + $.name"""))
        assert(!compiletime.testing.typeChecks("""$.age < "x" """))

    test("isNone tells missing values apart"):
        val plan = from(people).filter($.age.isNone || $.nickname.isNone).select(Tuple1($.name))
        assertEquals(plan.materialize(RowKernel()).map(_.name), List("bob", "carol"))
        assertEquals(from(people).filter(!$.age.isNone).select(Tuple1($.name)).materialize(RowKernel()).map(_.name), List("alice", "carol"))

    test("isNone works in a join residual"):
        val pets = List(Pet(Some("alice"), "cat"), Pet(None, "stray"))
        val plan = from(people).join(from(pets))($left.age.isNone && $right.owner.isNone).select(($.name, $.pet))
        assertEquals(plan.materialize(RowKernel()), List((name = "bob", pet = "stray")))
