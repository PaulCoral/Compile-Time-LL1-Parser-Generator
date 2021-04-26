import parser.parsingTable

@main def hello: Unit = 
    val parsed = parsingTable()
    println(s"Result : ${parsed}")    