package example.syntaxdef

import ll1compiletime.syntax.IdCounter
import ll1compiletime.syntax.Syntax
import ll1compiletime.syntax.Syntax._
import ll1compiletime.syntax.~
import ll1compiletime.syntax.SyntaxDefinition

import ll1compiletime.parser._

import scala.quoted._



inline def getPartialParsingTable = ${init}

def init(using Quotes) = {
    import SyntaxDef.given
    Expr(Parsing(SyntaxDef))
}


object SyntaxDef extends SyntaxDefinition[Int,MyToken,MyKind] {
    import MyToken._
    import MyKind._

    def getKind(t:MyToken):MyKind = MyKind.getKind(t)

    given idc:IdCounter = new IdCounter()

    lazy val elemInt: CSyntax[Int] = accept(IntKind){ case IntLitToken(v) => v }

    lazy val eof: CSyntax[Int] = accept(EOFKind){ case EOFToken => 0 }

    lazy val rec_sum: CSyntax[Int ~ Int] = (elemInt ~ sum)

    lazy val rec_sum_map: CSyntax[Int] = rec_sum.map{
        case a ~ b => a + b
    }

    lazy val sum: CSyntax[Int] = recursive { 
        rec_sum_map | eof
    }

    // Uncomment to get a LL1 Nullable error
    lazy val entryPoint = sum // | epsilon(1) | epsilon(0) 
}