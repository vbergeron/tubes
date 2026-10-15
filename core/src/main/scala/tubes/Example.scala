package tubes

case class User(id: Int, name: String, age: Int)

/** Rows read from a `Seq[User]`. */
type UserRow = NamedTuple.From[User]

/** Reusable step: keeps adults, minus the root account. */
def adults(users: Plan[UserRow]): Plan[UserRow] =
    users.filter(($.age >= 18) && ($.name =!= "root"))

/** Reusable step: projects each name alongside next year's age. */
def nextYear(users: Plan[UserRow]) =
    users.select(($.name, ($.age + 1).as("nextAge")))

@main def run =

    val people = List(
        User(1, "alice", 30),
        User(2, "root", 40),
        User(3, "bob", 17),
        User(4, "carol", 25)
    )

    // Steps compose with `through`, as long as the row shapes line up.
    val older = from(people)
        .through(adults)
        .through(nextYear)
        .filter($.nextAge > 30)

    println(older)
    for row <- older.materialize(RowKernel()) do println(s"${row.name} -> ${row.nextAge}")

    // Steps are plain functions, so they also compose on their own.
    val adultsNextYear = adults andThen nextYear

    val doubled = from(people)
        .through(adultsNextYear)
        .select(($.name, ($.nextAge * 2).as("doubled")))

    for row <- doubled.materialize(RowKernel()) do println(s"${row.name} -> ${row.doubled}")

    // A spec over an unbound input: any named tuple has a schema, so `from[A]` needs no data.
    val spec = from[UserRow].through(adultsNextYear)
    println(spec)

    val points = from[(x: Int, y: Int)].filter($.x < $.y).select(Tuple1(($.x + $.y).as("sum")))
    println(points)
