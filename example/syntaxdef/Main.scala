import example.syntaxdef._
import MyToken._

@main def hello: Unit =
    val parsingTable = getPartialParsingTable.withFunctionTable(SyntaxDef)

    val range = 1 to 5
    val head = range.head
    val tail = range.tail

    val str = tail.foldLeft(
            head.toString
        )((s,a) => 
            s"$s, ${a.toString}"
        )
    val tokens = NumberLexer(str)

    println("-------------------------------------")
    println(s" -> Lexing : ${str}")
    println(s" -> Parsing : ${tokens}")
    println(s" -> Result : ${parsingTable(tokens)}")
    println(s" -> Expected : ${range.sum}")
    println("--------------------------------------")
