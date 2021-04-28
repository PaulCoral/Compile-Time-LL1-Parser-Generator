package parser

import syntax.TokensAndKinds._
import ParsingTable.ParsingTableContext
import ParsingTable.ParsingTableInstruction
import ParsingTable.ParsingTableContext._
import ParsingTable.ParsingTableInstruction._

case class ParsingTable[A](entry: Int, table: Map[(Int,Kind), ParsingTableInstruction], nullable:Map[Int,Any]){
    private type Context = List[ParsingTableContext]
    private type Instruction = ParsingTableInstruction

    def apply(tokens: List[Token]) = {

    }

    private def parse(s: Int, c: Context, v:Option[Any], tokens: List[Token]): Option[Any] = {
        tokens match {
            case Nil => v
            case t::ts => {
                locate(t,s,c) match {
                    case None => None // no match, not nullable
                    case Some(Left(v)) => 
                        // nullable path taken
                        plugValue(v,c) match {
                            case (Left(value),c2) => 
                                // new value and new context
                                parse(s,c,Some(value),tokens)
                            case (Right(s2),c2) => 
                                // new found syntax, value saved in new context
                                parse(s2,c2,None,tokens)
                        }
                    case Some(Right(instr)) => 
                        // token kind matched => token consumed
                        instr match {
                            case Terminal => 
                                plugValue(t,c) match {
                                    case (Left(value),c2) => 
                                        // new value and new context
                                        parse(s,c2,Some(value),ts)
                                    case (Right(s2),c2) => 
                                        // new found syntax, value saved in new context
                                        parse(s2,c2,None,ts)
                            }

                            case NonTerminal(s2, cElem) => parse(s2, cElem::c,v,ts)
                        }
                }
            }
        }
    }

    private def locate(t: Token, s: Int, c: Context): Option[Either[Any, Instruction]] =
        table.get((s,t.toKind)) match {
            case None => 
                nullable.get(s) match {
                    case None => None
                    case Some(value) => Some(Left(value))
                }
            case Some(instr) => Some(Right(instr))
        }


    private def derive(t: Token, s: Int, c: Context) = {

    }


    private def plugValue(v:Any, c:Context): (Either[Any,Int],Context) = {
        c match {
            case Nil => (Left(v), Nil)
            case ApplyF(f)::cs => plugValue(f(v),cs)
            case PrependedBy(vp)::cs => plugValue((vp,v),cs)
            case FollowedBy(s)::cs => (Right(s),PrependedBy(v)::cs)
            case Passed::cs => plugValue(v,cs)
        }
    }
}

object ParsingTable{
    enum ParsingTableInstruction {
        case NonTerminal(id: Int, elem: ParsingTableContext) extends ParsingTableInstruction
        case Terminal extends ParsingTableInstruction
    }

    enum ParsingTableContext {
        case ApplyF(f: (Any) => Any) extends ParsingTableContext
        case PrependedBy(v: Any) extends ParsingTableContext
        case FollowedBy(s: Int) extends ParsingTableContext
        case Passed extends ParsingTableContext
    }
}