import example.syntaxdef.SyntaxDef

@main def hello: Unit = 
    val a = SyntaxDef.parse
    println(s"Result : ${a}")    
