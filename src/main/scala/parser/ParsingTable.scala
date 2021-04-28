package parser


import scala.quoted._
import scala.quoted.ToExpr._


import syntax.TokensAndKinds._
import ParsingTable.ParsingTableContext._
import ParsingTable.ParsingTableInstruction._
import ParsingTable._

case class ParsingTable[A](entry: Int, table: Map[(Int,Kind), ParsingTableInstruction], nullable:Map[Int,Any]){
    private type Context = List[ParsingTableContext]
    private type Instruction = ParsingTableInstruction

    def apply(tokens: List[Token]):ParsingResult[A] = {
        parse(entry,List(),tokens).toType[A]
    }

    private def parse(s: Int, c: Context, tokens: List[Token]): ParsingResult[Any] = {
        //throw Exception(table.toString)
        tokens match {
            case Nil => 
                nullable.get(s) match {
                    case None => UnexpectedEnd(getFirstSetOfSyntax(s))
                    case Some(v) => plugValue(v,c) match {
                        case Left(v) => result(v,Nil)
                        case Right((s2,c2)) => parse(s,c,Nil)
                    }
                }
            case t::ts => {
                locate(t,s,c) match {
                    case None => UnexpectedToken(t.toKind,getFirstSetOfSyntax(s)) // no match, not nullable
                    case Some(Left(v)) => 
                        // nullable path taken
                        plugValue(v,c) match {
                            case Left(value) => 
                                // context is empty, parsing terminated
                                result(value,tokens)
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
                                        result(value,ts)
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

    private def result(v: Any, tokens: List[Token]) = tokens match {
        case Nil => ParsedSuccessfully(v)
        case _ => ParsedSuccessfullyWithRest(v,tokens)
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


    private def getFirstSetOfSyntax(s: Int):Set[Kind] = {
        table.keys.filter(_._1 == s).map(_._2).toSet
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

    object ParsingTableInstruction {
        given ToExpr[ParsingTableInstruction] with {
            def apply(pti : ParsingTableInstruction)(using Quotes):Expr[ParsingTableInstruction] = 
                pti match {
                    case Terminal => Expr(Terminal)
                    case NonTerminal(i,e) => '{NonTerminal(${Expr(i)}, ${Expr(e)})}
                }
        }
    }

    enum ParsingTableContext {
        case ApplyF(f: (Any) => Any) extends ParsingTableContext
        case PrependedBy(v: Any) extends ParsingTableContext
        case FollowedBy(s: Int) extends ParsingTableContext
        case Passed extends ParsingTableContext
    }

    object ParsingTableContext {
        given ToExpr[ParsingTableContext] with {
            def apply(ptc:ParsingTableContext)(using Quotes) = ptc match{
                case ApplyF(f) => '{ApplyF(${Expr(f)})}
                case PrependedBy(v) => ???
                case FollowedBy(s) => ???
                case Passed => ???
            }
        }
    }

    sealed trait ParsingResult[A] {
        def toType[B]:ParsingResult[B] = this match {
            case ParsedSuccessfully(v) => ParsedSuccessfully(v.asInstanceOf[B])
            case ParsedSuccessfullyWithRest(v,r) => ParsedSuccessfullyWithRest(v.asInstanceOf[B],r)
            case UnexpectedEnd(e) => UnexpectedEnd[B](e)
            case UnexpectedToken(k,e) => UnexpectedToken[B](k,e)
        }

        override def toString = this match {
            case ParsedSuccessfully(v) =>
                s"Successful parsing: $v"
            case ParsedSuccessfullyWithRest(v,r) =>
                s"Successful parsing with rest. Value : $v\nRest: $r"
            case UnexpectedEnd(e) => 
                s"Unexpected End, expected kind : $e"
            case UnexpectedToken(k,e) => 
                s"Unexpected Token of Kind $k, expected kind : $e"
        }
    }
    case class ParsedSuccessfully[A](v: A) extends ParsingResult[A]
    case class ParsedSuccessfullyWithRest[A](v: A, tokens:List[Token]) extends ParsingResult[A]
    case class UnexpectedEnd[A](expected: Set[Kind]) extends ParsingResult[A]
    case class UnexpectedToken[A](k: Kind, expected: Set[Kind]) extends ParsingResult[A]

    given ParsingTableToExpr[A: Type: ToExpr]: ToExpr[ParsingTable[A]] with {
        import Kind._
        def apply(pt: ParsingTable[A])(using Quotes): Expr[ParsingTable[A]] =
            '{ParsingTable(${Expr(pt.entry)},${Expr(pt.table)},${Expr(pt.nullable)})} 
    }

    given ToExpr[Any] with {
        def apply(any : Any)(using Quotes): Expr[Any] = any match {
            case x:Int => Expr(x)
        }
    }
}