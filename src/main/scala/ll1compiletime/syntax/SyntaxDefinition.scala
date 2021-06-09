package ll1compiletime.syntax

import ll1compiletime.parser.PartialParsingTable
import ll1compiletime.parser.ParsingTable

import scala.quoted.ToExpr

trait SyntaxDefinition[A,T,K] {
    type Token = T
    type Kind = K

    type CSyntax[X] = Syntax[X,Token,Kind]

    def getKind(t: Token):Kind

    lazy val entryPoint: CSyntax[A]

    inline def parse:ParsingTable[A,Token,Kind]

    given anyToExpr : ToExpr[Any]

    /* 
     * Syntax Context : we restrain the context of the syntax for tokens and kinds   
     */

    def accept[B](k:Kind)(f: PartialFunction[Token,B])(using IdCounter): CSyntax[B] =
        Syntax.accept(k)(f)

    def epsilon[B](e: B)(using IdCounter): CSyntax[B] = 
        Syntax.epsilon[B,Token,Kind](e)

    def elem(k: Kind)(using IdCounter): CSyntax[Token] = 
        Syntax.elem[Token,Kind](k:Kind)

    def recursive[B](syntax: => CSyntax[B])(using IdCounter): CSyntax[B] = 
        Syntax.recursive[B,Token,Kind](syntax)

    extension [X](thiz: CSyntax[X]) {
        infix def |(that: CSyntax[X])(using IdCounter):CSyntax[X] = 
            thiz.|(that)

        infix def ~[B](that: CSyntax[B]):CSyntax[(X,B)] = 
            thiz.~(that)

        /**
         * Sequence operator, keeping the left value
         */
        infix def ~<~[B](that: CSyntax[B]):CSyntax[X] = 
            thiz.~<~(that)
        /**
         * Sequence operator, keeping the right value
         */
        infix def ~>~[B](that: CSyntax[B]):CSyntax[B] = 
            thiz.~>~(that)

        /**
         * Map this syntax to another
         */
        def map[B](f: X => B): CSyntax[B] =
            thiz.map(f)
    }
        
}
