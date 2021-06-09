package ll1compiletime.parser

import ParsingTable.ParsingTableInstruction
import ll1compiletime.syntax.Syntax
import ll1compiletime.syntax.SyntaxDefinition

import scala.quoted._



case class PartialParsingTable[A,Kind](entry: Int, table: Map[(Int,Kind), ParsingTableInstruction], nullable:Map[Int,Any]){
    def withFunctionTable[Token](ft : Map[Int,(Any => Any)],getKind: Token => Kind):ParsingTable[A,Token,Kind] = 
        ParsingTable[A,Token,Kind](entry, table,nullable,ft,getKind)

    def withFunctionTable[Token](s: Syntax[A,Token,Kind], getKind: Token => Kind):ParsingTable[A,Token,Kind] = 
        withFunctionTable(Syntax.idToFunc(s),getKind)
        
    def withFunctionTable[Token](sd: SyntaxDefinition[A,Token,Kind]):ParsingTable[A,Token,Kind] = 
        withFunctionTable(Syntax.idToFunc(sd.entryPoint),sd.getKind)
}

object PartialParsingTable {
    given PartialParsingTableToExpr[A: Type: ToExpr, Kind: Type: ToExpr](using a:ToExpr[Any]): ToExpr[PartialParsingTable[A,Kind]] with {
        def apply(pt: PartialParsingTable[A,Kind])(using Quotes): Expr[PartialParsingTable[A,Kind]] = {
            '{PartialParsingTable(${Expr(pt.entry)},${Expr(pt.table)},${Expr(pt.nullable)})} 
        }
    }
}