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
