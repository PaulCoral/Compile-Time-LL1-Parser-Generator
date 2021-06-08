import example.syntaxdef.SyntaxDef

import syntax.TokensAndKinds.Token._

@main def hello: Unit = 
    val a = SyntaxDef.parse
    val tokens = (1 to 10).map(IntLitToken(_)).toList
    println(s"Result : ${a(tokens)}")    
