package ll1compiletime.parser


import scala.quoted._
import scala.quoted.ToExpr._

import ParsingTable.ParsingTableContext._
import ParsingTable.ParsingTableInstruction._
import ParsingTable._

case class ParsingTable[A,Token,Kind](
    entry: Int, 
    table: Map[(Int,Kind), ParsingTableInstruction], 
    nt:Map[Int,Any],
    nullable: Map[Int,Nullable],
    ft:Map[Int,(Any => Any)],
    getKind: Token => Kind
){

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
                    case Some(v) => plugValue(v.get(using nt),c) match {
                        case Left(v) => result(v,Nil)
                        case Right((s2,c2)) => parse(s,c,Nil)
                    }
                }
            case t::ts => {
                locate(t,s,c) match {
                    case None => UnexpectedToken(getKind(t),getFirstSetOfSyntax(s)) // no match, not nullable
                    case Some(Left(v)) => 
                        // nullable path taken
                        plugValue(v,c) match {
                            case Left(value) => 
                                // context is empty, parsing finished
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
        table.get((s,getKind(t))) match {
            case None => 
                nullable.get(s) match {
                    case None => None
                    case Some(value) => Some(Left(value.get(using nt)))
                }
            case Some(instr) => Some(Right(instr))
        }


    private def getFirstSetOfSyntax(s: Int):Set[Kind] = {
        table.keys.filter(_._1 == s).map(_._2).toSet
    }


    private def plugValue(v:Any, c:Context): Either[Any,(Int,Context)] = {
        c match {
            case Nil => Left(v)
            case ApplyF(fId)::cs => plugValue(ft(fId)(v),cs)
            case PrependedBy(vp)::cs => plugValue((vp,v),cs)
            case PrependedByNullable(s)::cs => plugValue((nullable(s).get(using nt),v),cs)
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
        given ParsingTableInstructionToExpr: ToExpr[ParsingTableInstruction] with {
            def apply(pti : ParsingTableInstruction)(using Quotes):Expr[ParsingTableInstruction] = 
                pti match {
                    case Terminal => '{Terminal}
                    case NonTerminal(i,e) => '{NonTerminal(${Expr(i)}, ${Expr(e)})}
                }
        }
    }

    enum ParsingTableContext {
        case ApplyF(id: Int) extends ParsingTableContext
        case PrependedBy(v: Any) extends ParsingTableContext
        case PrependedByNullable(s: Int) extends ParsingTableContext
        case FollowedBy(s: Int) extends ParsingTableContext
        case Passed extends ParsingTableContext
    }

    object ParsingTableContext {
        private given ToExpr[(Any => Any)] with {
            def apply(f : Any => Any)(using Quotes) = 
                '{(a:Any) => 
                    val b = a.asInstanceOf[(Int,Int)]
                    b._1 + b._2
                }
        }

        given ParsingTableContextToExpr : ToExpr[ParsingTableContext] with {
            def apply(ptc:ParsingTableContext)(using Quotes) = ptc match{
                case ApplyF(f) => '{ApplyF(${Expr(f)})}
                case PrependedBy(_) => throw UnsupportedOperationException(s"This < ${ptc} > should not be used at compile time")
                case PrependedByNullable(s) => '{PrependedByNullable(${Expr(s)})}
                case FollowedBy(s) => '{FollowedBy(${Expr(s)})}
                case Passed => '{Passed}
            }
        }
    }

    sealed trait Nullable {
        def get(using table: Map[Int,Any]):Any
    }
    object Nullable {
        given ToExpr[Nullable] with {
            def apply(n: Nullable)(using Quotes): Expr[Nullable] = n match {
                case Leaf(i) => '{Leaf(${Expr(i)})}
                case Node(l,r) => '{Node(${Expr(l)},${Expr(r)})}
            }
        }
    }
    case class Leaf(i:Int) extends Nullable{
        def get(using table: Map[Int,Any]):Any = i.asInstanceOf[Any]
    }
    case class Node(left: Nullable, right: Nullable) extends Nullable {
        def get(using table: Map[Int,Any]):Any = ((left.get, right.get)).asInstanceOf[Any]
    }

    sealed trait ParsingResult[A] {
        def toType[B]:ParsingResult[B]

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
    case class ParsedSuccessfully[A](v: A) extends ParsingResult[A] {
        def toType[B]:ParsingResult[B] = ParsedSuccessfully(v.asInstanceOf[B])
    }
    case class ParsedSuccessfullyWithRest[A,Token](v: A, tokens:List[Token]) extends ParsingResult[A]{
        def toType[B]:ParsingResult[B] = ParsedSuccessfullyWithRest(v.asInstanceOf[B],tokens)
    }
    case class UnexpectedEnd[A,Kind](expected: Set[Kind]) extends ParsingResult[A]{
        def toType[B]:ParsingResult[B] = UnexpectedEnd[B,Kind](expected)
    }
    case class UnexpectedToken[A,Kind](k: Kind, expected: Set[Kind]) extends ParsingResult[A]{
        def toType[B]:ParsingResult[B] = UnexpectedToken[B,Kind](k,expected)
    }
}