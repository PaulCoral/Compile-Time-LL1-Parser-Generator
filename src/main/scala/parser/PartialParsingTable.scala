package parser

import ParsingTable.ParsingTableInstruction
import syntax.TokensAndKinds.Kind
import syntax.Syntax

import scala.quoted._
import syntax.SyntaxDefinition


case class PartialParsingTable[A](entry: Int, table: Map[(Int,Kind), ParsingTableInstruction], nullable:Map[Int,Any]){
    def withFunctionTable(ft : Map[Int,(Any => Any)]):ParsingTable[A] = 
        ParsingTable(entry, table,nullable,ft)

    def withFunctionTable(s: Syntax[A]):ParsingTable[A] = 
        withFunctionTable(Syntax.idToFunc(s))
        
    def withFunctionTable(sd: SyntaxDefinition[A]):ParsingTable[A] = 
        withFunctionTable(Syntax.idToFunc(sd.entryPoint))
}

object PartialParsingTable {
    given PartialParsingTableToExpr[A: Type: ToExpr](using a:ToExpr[Any]): ToExpr[PartialParsingTable[A]] with {
        import Kind._
        def apply(pt: PartialParsingTable[A])(using Quotes): Expr[PartialParsingTable[A]] = {
            '{PartialParsingTable(${Expr(pt.entry)},${Expr(pt.table)},${Expr(pt.nullable)})} 
        }
    }
}