
package ll1compiletime.parser

import ParsingTable.{SymboleType,Nullable}
import ll1compiletime.syntax.{Syntax, SyntaxDefinition}

import scala.quoted._


/**
 * A partial parsing table. It doesn't contain the produced value.
 * 
 * It cannot be used as is, the value should be provided with 
 * the method `withFunctionTable(...)`
 * 
 * @tparam Kind the Token Kind used in parsing
 * @param entry the id of the entry point syntax
 * @param table a Map from the id of the current syntax to parse along 
 * with the Kind of the current parsed Token and gives back the next 
 * parsing instruction
 * @param nullable a Map from the syntax id (which is nullable) to the
 * actual value (or a reference to the syntax that hold them)
 * 
 * @note This class shouldn't be used, it just can't be made
 * private to the `ll1compiletime` package because it is inlined
 * by the macros
 *              
 */
class PartialParsingTable[Kind] (
    private val entry: Int,
    private val table: Map[(Int,Kind), SymboleType],
    private val nullable: Map[Int,Nullable]
){

    private def withFunctionTable[A,Token](ft : Map[Int,(Any => Any)],nt:Map[Int,Any],getKind: Token => Kind):ParsingTable[A,Token,Kind] = 
        new ParsingTable[A,Token,Kind](entry, table,nt,nullable,ft,getKind)

    private def withFunctionTable[A,Token](s: Syntax[A,Token,Kind],getKind: Token => Kind):ParsingTable[A,Token,Kind] = 
        val (ft, nt) = Syntax.runtimeSyntaxData(s)
        withFunctionTable(ft, nt, getKind)
        
    /**
     * Return a complete ParsingTable, by providing values to the partial one
     * 
     * @tparam A the type of the value produced by the entry syntax
     * @tparam Token the type of the Tokens used in parsing
     * @param sd the SyntaxDefinition which values are used to complete the partial table
     */
    def withFunctionTable[A,Token](sd: SyntaxDefinition[A,Token,Kind]):ParsingTable[A,Token,Kind] = 
        require(entry == sd.entryPoint.id)
        withFunctionTable(sd.entryPoint,sd.getKind)
}

object PartialParsingTable {
    /** 
     * A given which is used to get the `quoted.Expr` of this partial table
     * 
     * It is used to get the value out of a macro
     */
    given PartialParsingTableToExpr[Kind: Type: ToExpr]: ToExpr[PartialParsingTable[Kind]] with {
        def apply(pt: PartialParsingTable[Kind])(using Quotes): Expr[PartialParsingTable[Kind]] = {
            '{PartialParsingTable(${Expr(pt.entry)},${Expr(pt.table)},${Expr(pt.nullable)})} 
        }
    }
}