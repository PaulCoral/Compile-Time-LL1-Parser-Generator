import example.syntaxdef.SyntaxDef

import syntax.TokensAndKinds.Token._

@main def hello: Unit = 
    val a = SyntaxDef.parse
    val tokens = List(IntLitToken(1),IntLitToken(2))
    println(s"Result : ${a(tokens)}")    
