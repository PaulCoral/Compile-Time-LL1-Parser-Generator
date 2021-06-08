package syntax

import parser.PartialParsingTable
import parser.ParsingTable

trait SyntaxDefinition[A] {
    lazy val entryPoint:Syntax[A]

    inline def parse:ParsingTable[A]
}
