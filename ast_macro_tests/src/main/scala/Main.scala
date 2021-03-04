import ast._

@main def hello: Unit = {
    println("Hello world!")
    println(s"$tree = ${tree.eval}")
    println("finsihed")
}

def tree = 
    Plus(
        Const(1),
        Minus(
            Const(10),
            Div(
                Times(
                    Const(2),
                    Const(2)
                ),
                Const(2)
            )
        )
    )
