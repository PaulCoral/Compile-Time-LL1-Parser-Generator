package ll1compiletime.parser

import ParsingTable.{ParsingTableInstruction,Nullable}
import ll1compiletime.syntax.{Syntax, SyntaxDefinition}

import scala.quoted._



case class PartialParsingTable[A,Kind](entry: Int, table: Map[(Int,Kind), ParsingTableInstruction], nullable: Map[Int,Nullable]){

    def withFunctionTable[Token](ft : Map[Int,(Any => Any)],nt:Map[Int,Any],getKind: Token => Kind):ParsingTable[A,Token,Kind] = 
        ParsingTable[A,Token,Kind](entry, table,nt,nullable,ft,getKind)

    def withFunctionTable[Token](s: Syntax[A,Token,Kind],getKind: Token => Kind):ParsingTable[A,Token,Kind] = 
        val (ft, nt) = Syntax.runtimeSyntaxData(s)
        withFunctionTable(ft, nt, getKind)
        
    def withFunctionTable[Token](sd: SyntaxDefinition[A,Token,Kind]):ParsingTable[A,Token,Kind] = 
        withFunctionTable(sd.entryPoint,sd.getKind)
}

object PartialParsingTable {
    given PartialParsingTableToExpr[A: Type: ToExpr, Kind: Type: ToExpr]: ToExpr[PartialParsingTable[A,Kind]] with {
        def apply(pt: PartialParsingTable[A,Kind])(using Quotes): Expr[PartialParsingTable[A,Kind]] = {
            '{PartialParsingTable(${Expr(pt.entry)},${Expr(pt.table)},${Expr(pt.nullable)})} 
        }
    }
}