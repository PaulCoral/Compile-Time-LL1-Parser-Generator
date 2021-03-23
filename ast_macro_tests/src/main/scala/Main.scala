import ast._
import scala.io.StdIn.readInt
@main def hello: Unit = {
    println("Hello world!")
    println(nb)
    println("finsihed")
}


val nb:6 = eval(tree)

transparent inline def tree = 
    Plus(
        Const(1),
        Plus(
            Const(2),
            Const(3)
        )
    )