package syntax

import parser.PartialParsingTable

trait SyntaxDefinition[A] {
    given id:IdCounter

    lazy val entryPoint:Syntax[A]
}
