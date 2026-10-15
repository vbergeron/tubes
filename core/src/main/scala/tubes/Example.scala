package tubes

case class User(id: Int, name: String, age: Int)

/** Rows read from a `Seq[User]`. */
type UserRow = NamedTuple.From[User]

/** Reusable step: keeps adults, minus the root account. Inline, so the row kernel can compile it into each plan. */
inline def adults(inline users: Plan[UserRow]): Plan[UserRow] =
    users.filter(($.age >= 18) && ($.name =!= "root"))

/** Reusable step: projects each name alongside next year's age. */
inline def nextYear(inline users: Plan[UserRow]) =
    users.select(($.name, ($.age + 1).as("nextAge")))

@main def run =

    val people = List(
        User(1, "alice", 30),
        User(2, "root", 40),
        User(3, "bob", 17),
        User(4, "carol", 25)
    )

    // Steps compose with `through`, as long as the row shapes line up.
    inline def older = from(people)
        .through(adults)
        .through(nextYear)
        .filter($.nextAge > 30)

    println(older)
    for row <- older.materialize(RowKernel) do println(s"${row.name} -> ${row.nextAge}")

    val doubled = from(people)
        .through(adults)
        .through(nextYear)
        .select(($.name, ($.nextAge * 2).as("doubled")))
        .materialize(RowKernel)

    for row <- doubled do println(s"${row.name} -> ${row.doubled}")

    // A spec over an unbound input: any named tuple has a schema, so `from[A]` needs no data.
    val spec = from[UserRow].through(adults).through(nextYear)
    println(spec)

    val points = from[(x: Int, y: Int)].filter($.x < $.y).select(Tuple1(($.x + $.y).as("sum")))
    println(points)
