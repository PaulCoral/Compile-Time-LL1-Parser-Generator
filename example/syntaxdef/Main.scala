import example.syntaxdef._
import MyToken._

/** the main function */
@main def hello: Unit =
    // parsing table constructed at runtime 
    // `withFunctionTable` provide the need runtime values
    val parsingTable = getPartialParsingTable.withFunctionTable(SyntaxDef)

    val range = 1 to 5
    val head = range.head // 1
    val tail = range.tail // 2 to 5

    // we construct a string to parse
    val str = tail.foldLeft(
            head.toString
        )((s,a) => 
            s"$s, ${a.toString}"
        )

    // lexing the string `str` into tokens
    val tokens = NumberLexer(str) 

    // parsing the tokens into a value
    val result = parsingTable(tokens)

    //printing the all the values
    println("-------------------------------------")
    println(s" -> Lexing : ${str}")
    println(s" -> Result : ${result}")
    println(s" -> Expected : ${range.sum}")
    println("--------------------------------------")
