package ll1compiletime.parser

import ParsingTable.{ParsingTableInstruction,Nullable}
import ll1compiletime.syntax.{Syntax, SyntaxDefinition}

import scala.quoted._



case class PartialParsingTable[Kind](entry: Int, table: Map[(Int,Kind), ParsingTableInstruction], nullable: Map[Int,Nullable]){

    private def withFunctionTable[A,Token](ft : Map[Int,(Any => Any)],nt:Map[Int,Any],getKind: Token => Kind):ParsingTable[A,Token,Kind] = 
        ParsingTable[A,Token,Kind](entry, table,nt,nullable,ft,getKind)

    private def withFunctionTable[A,Token](s: Syntax[A,Token,Kind],getKind: Token => Kind):ParsingTable[A,Token,Kind] = 
        val (ft, nt) = Syntax.runtimeSyntaxData(s)
        withFunctionTable(ft, nt, getKind)
        
    def withFunctionTable[A,Token](sd: SyntaxDefinition[A,Token,Kind]):ParsingTable[A,Token,Kind] = 
        withFunctionTable(sd.entryPoint,sd.getKind)
}

object PartialParsingTable {
    given PartialParsingTableToExpr[Kind: Type: ToExpr]: ToExpr[PartialParsingTable[Kind]] with {
        def apply(pt: PartialParsingTable[Kind])(using Quotes): Expr[PartialParsingTable[Kind]] = {
            '{PartialParsingTable(${Expr(pt.entry)},${Expr(pt.table)},${Expr(pt.nullable)})} 
        }
    }
}