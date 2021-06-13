package example.syntaxdef

import ll1compiletime._

import scala.quoted._

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
inline def getPartialParsingTable = ${init}

/**
 * The macro computing the partial parsing table at compile time
 */ 
def init(using Quotes) = {
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

    // ----- The Syntax definition itself ---------

    lazy val elemInt: CSyntax[Int] = accept(IntKind){ case IntLitToken(v) => v }

    lazy val eof: CSyntax[Int] = accept(EOFKind){ case EOFToken => 0 }

    lazy val rec_sum: CSyntax[Int ~ Int] = (elemInt ~ sum)

    lazy val rec_sum_map: CSyntax[Int] = rec_sum.map{
        case a ~ b => a + b
    }

    lazy val sum: CSyntax[Int] = recursive { 
        rec_sum_map | eof
    }

    // --------------------------------------------

    // Uncomment to get a LL1 Nullable error
    lazy val entryPoint = sum // | epsilon(1) | epsilon(0) 
}