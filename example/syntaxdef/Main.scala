import example.syntaxdef._
import MyToken._

@main def hello: Unit =
    val parsingTable = SyntaxDef.parse

    val range = 1 to 10
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
