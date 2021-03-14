import ast._

@main def hello: Unit = {
    println("Hello world!")
    println(tree)
    println(tree.eval)
    println("finsihed")
}

def tree = 
    Plus(
        Var("two",Const(1)),
        Const(1)
    )