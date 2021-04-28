import parser.parsingTable

@main def hello: Unit = 
    val parsed = parsingTable(true)
    println(s"Result : ${parsed}")    