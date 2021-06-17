package example.syntaxdef

import ll1compiletime._

import scala.quoted._
import scala.language.implicitConversions
import ll1compiletime.parser.PartialParsingTable

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
object SyntaxDef extends SyntaxDefinition[Int,MyToken,MyKind] {
    import MyToken._
    import MyKind._

    def getKind(t:MyToken):MyKind = MyKind.getKind(t)

    inline def macroCall:PartialParsingTable[Kind] = getPartialParsingTable

    given Conversion[Char,CSyntax[Token]] with {
        def apply(c: Char) = elem(SeparatorKind)
    }

    // ----- The Syntax definition itself ---------

    lazy val elemInt: CSyntax[Int] = accept(IntKind){ case IntLitToken(v) => v }

    lazy val eof: CSyntax[Int] = accept(EOFKind){ case EOFToken => 0 }

    lazy val rec_sum: CSyntax[Int ~ Int] = (elemInt ~ sum)

    lazy val rec_sum_map: CSyntax[Int] = rec_sum.map{
        case a ~ b => a + b
    }

    lazy val sum: CSyntax[Int] = repsep(elemInt,',').map(_.sum)

    // --------------------------------------------

    // Uncomment to get a LL1 Nullable error
    lazy val entryPoint = sum ~<~ elem(EOFKind) // | epsilon(1) | epsilon(0) 
}