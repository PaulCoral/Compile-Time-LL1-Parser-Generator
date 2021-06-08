import example.syntaxdef._

import syntax.TokensAndKinds.Token._

import parser.ParsingTable

@main def hello: Unit =
    val a = SyntaxDef.parse
    val tokens = (1 to 3).map(IntLitToken(_)).toList
    println(s"Result : ${a(tokens)}")
