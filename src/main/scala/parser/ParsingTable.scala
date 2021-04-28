package parser

import syntax.TokensAndKinds._
import ParsingTable.ParsingTableContext
import ParsingTable.ParsingTableContext._
import ParsingTable.ParsingTableInstruction
import ParsingTable.ParsingTableInstruction._
import ParsingTable.ParsingResult
import ParsingTable.ParsingResult._

case class ParsingTable[A](entry: Int, table: Map[(Int,Kind), ParsingTableInstruction], nullable:Map[Int,Any]){
    private type Context = List[ParsingTableContext]
    private type Instruction = ParsingTableInstruction

    def apply(tokens: List[Token]):A = {
        parse(entry,List(),tokens)
    }

    private def parse(s: Int, c: Context, tokens: List[Token]): ParsingResult = {
        tokens match {
            case Nil => UnexpectedEnd()
            case t::ts => {
                locate(t,s,c) match {
                    case None => UnexpectedToken(t.toKind) // no match, not nullable
                    case Some(Left(v)) => 
                        // nullable path taken
                        plugValue(v,c) match {
                            case Left(value) => 
                                // context is empty, parsing terminated
                                result(Some(value),tokens)//parse(s,c,Some(value),tokens)
                            case Right((s2,c2)) => 
                                // new found syntax, value saved in new context
                                parse(s2,c2,tokens)
                        }
                    case Some(Right(instr)) => 
                        // token kind matched
                        instr match {
                            case Terminal => 
                                // TOKEN CONSUMED
                                plugValue(t,c) match {
                                    case Left(value) => 
                                        // value and context empty Finished
                                        result(Some(value),ts)//parse(s,c2,Some(value),ts)
                                    case Right((s2,c2)) => 
                                        // new found syntax, value saved in new context
                                        parse(s2,c2,ts)
                            }

                            case NonTerminal(s2, cElem) => parse(s2, cElem::c,tokens)
                        }
                }
            }
        }
    }

    private def result(v: Option[Any], tokens: List[Token]) = v match {
        case None => 
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


    private def plugValue(v:Any, c:Context): Either[Any,(Int,Context)] = {
        c match {
            case Nil => Left(v)
            case ApplyF(f)::cs => plugValue(f(v),cs)
            case PrependedBy(vp)::cs => plugValue((vp,v),cs)
            case FollowedBy(s)::cs => Right((s,PrependedBy(v)::cs))
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

    enum ParsingResult[A] {
        case ParsedSuccessfully[A](v: A) extends ParsingResult[A]
        case UnexpectedEnd[A](expected: Set[Kind]) extends ParsingResult[A]
        case UnexpectedToken[A](k: Kind, expected: Set[Kind]) extends ParsingResult[A]

        def toType[B]:ParsingResult[B] = this match {
            case ParsedSuccessfully(v) => ParsedSuccessfully(v.asInstanceOf[B])
            case UnexpectedEnd(e) => UnexpectedEnd[B](e)
            case UnexpectedToken(k,e) => UnexpectedToken[B](k,e)
        }
    }
}