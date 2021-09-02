package example.modular

import ll1compiletime._

import scala.quoted._
import scala.language.implicitConversions
import ll1compiletime.parser.PartialParsingTable
import javax.print.attribute.standard.MediaSize.Other


/*
 * This is a modular version of the other example. The idea is to reuse or mix
 * multiple syntax definition to avoid code duplication
 */

// ==============================================================================
// ==============================================================================
// ==============================================================================

/*
 * !!! READ THIS !!! 
 * 
 * THE SYNTAX DEFINITION AND THE MACRO SHOULD BE IN THE SAME FILE,
 * OTHERWISE IF YOU CHANGE THE SYNTAX, THE MACRO WON'T BE RECOMPILED.
 * 
 * THE SYNTAX DEFINITION OBJECT SHOULD NOT CALL THE MACRO FUNCTION, AS A MACRO
 * CANNOT BE CALLED IN THE SAME FILE IT IS DEFINED.
 */

/**
 * Give the partial parsing table at compile time
 */ 
private inline def getPartialParsingTable:PartialParsingTable[SyntaxDef.Kind] = ${init}

/**
 * The macro computing the partial parsing table at compile time
 */ 
private def init(using Quotes):Expr[PartialParsingTable[SyntaxDef.Kind]] = {
    Expr(buildParsingTable(SyntaxDef))
}

/**
 * The Syntax definition
 * 
 * - Result of type `Int`
 * - Token of type `MyToken`
 * - Kind of tpye `MyKind`
 */
trait SomeSyntax extends SyntaxDefinition[MyToken,MyKind] {
    import MyToken._
    import MyKind._

    given Conversion[Char,CSyntax[Token]] with {
        def apply(c: Char) = elem(SeparatorKind)
    }

    // ----- The Syntax definition itself ---------

    lazy val elemInt: CSyntax[Int] = accept(IntKind){ case IntLitToken(v) => v }

    lazy val rec_sum: CSyntax[Int ~ Int] = (elemInt ~ sum)

    lazy val rec_sum_map: CSyntax[Int] = rec_sum.map{
        case a ~ b => a + b
    }

    lazy val sum: CSyntax[Int] = repsep(elemInt,',').map(_.sum)
}


trait OtherSyntax extends SyntaxDefinition[MyToken,MyKind] {
    import MyToken._
    import MyKind._

    val eof = elem(EOFKind)
}

object SyntaxDef extends CompileTime[Int,MyToken,MyKind] with SomeSyntax with OtherSyntax {
    import MyToken._
    import MyKind._

    def getKind(t:MyToken):MyKind = MyKind.getKind(t)

    inline def macroCall:PartialParsingTable[Kind] = getPartialParsingTable

    lazy val entryPoint = sum ~<~ eof
}