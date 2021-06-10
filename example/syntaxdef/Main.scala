import example.syntaxdef._
import MyToken._

@main def hello: Unit =
    val a = SyntaxDef.parse

    // compute the sum of number from 1 to 10
    val tokens = (1 to 10).map(IntLitToken(_)).toList
    println(s"Result : ${a(tokens)}")
