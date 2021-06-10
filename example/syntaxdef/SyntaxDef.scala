package example.syntaxdef

import ll1compiletime.syntax.IdCounter
import ll1compiletime.syntax.Syntax
import ll1compiletime.syntax.Syntax._
import ll1compiletime.syntax.SyntaxDefinition

import ll1compiletime.parser._

import scala.quoted._


object SyntaxDef extends SyntaxDefinition[Int,MyToken,MyKind] {
    import MyToken._
    import MyKind._

    def getKind(t:MyToken):MyKind = MyKind.getKind(t)

    given id:IdCounter = new IdCounter()

    lazy val elemInt: CSyntax[Int] = accept(IntKind){ case IntLitToken(v) => v }

    lazy val rec_sum: CSyntax[Int] = recursive{ sum | epsilon(0) }

    lazy val sum: CSyntax[Int] = (elemInt ~ rec_sum).map{ case (a,b) => a + b }


    lazy val entryPoint = sum

    inline def parse = getPartialParsingTable.withFunctionTable(this)

    given anyToExpr:ToExpr[Any] with {
        def apply(a: Any)(using Quotes) = a match {
            case x:Int => Expr(x)
            case x:String => Expr(x)
        }
    }
}



