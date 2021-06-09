package ll1compiletime.syntax

import ll1compiletime.parser.PartialParsingTable
import ll1compiletime.parser.ParsingTable

import scala.quoted.ToExpr

trait SyntaxDefinition[A,T,K] {
    type Token = T
    type Kind = K

    def getKind(t: Token):Kind

    lazy val entryPoint:Syntax[A,Token,Kind]

    inline def parse:ParsingTable[A,Token,Kind]

    given anyToExpr : ToExpr[Any]

    /* 
     * Syntax Context : we restrain the context of the syntax for tokens and kinds   
     */

    def accept[B](k:Kind)(f: PartialFunction[Token,B])(using IdCounter): Syntax[B,Token,Kind] =
        Syntax.accept(k)(f)

    def epsilon[B](e: B)(using IdCounter): Syntax[B,Token,Kind] = 
        Syntax.epsilon[B,Token,Kind](e)

    def elem(k: Kind)(using IdCounter): Syntax[Token,Token,Kind] = 
        Syntax.elem[Token,Kind](k:Kind)

    def recursive[B](syntax: => Syntax[B,Token,Kind])(using IdCounter): Syntax[B,Token,Kind] = 
        Syntax.recursive[B,Token,Kind](syntax)
}
