package ll1compiletime.syntax

import parser.PartialParsingTable
import parser.ParsingTable

import scala.quoted.ToExpr

trait SyntaxDefinition[A,T,K] {
    type Token = T
    type Kind = K

    def getKind(t: Token):Kind

    lazy val entryPoint:Syntax[A,Token,Kind]

    inline def parse:ParsingTable[A,Token,Kind]

    given anyToExpr : ToExpr[Any]
}
