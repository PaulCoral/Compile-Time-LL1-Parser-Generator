import example.syntaxdef._
import MyToken._

import parser.ParsingTable

@main def hello: Unit =
    val a = SyntaxDef.parseRuntime
    val tokens = (1 to 10000).map(IntLitToken(_)).toList
    println(s"Result : ${a(tokens)}")
