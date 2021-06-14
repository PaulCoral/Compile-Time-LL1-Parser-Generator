package ll1compiletime.parser


import scala.quoted._
import scala.quoted.ToExpr._
import scala.annotation.tailrec

import ll1compiletime._

import ParsingTable.ParsingTableContext._
import ParsingTable.SymboleType._
import ParsingTable._

/**
 * A parsing table which can return a result
 * from a token sequence
 * 
 * @param entry the id of the entry syntax
 * @param table A Map from a syntax and the parsed Token Kind to
 * a parsing instruction.
 * @param nt nullable table that map an id to a value
 * @param nullable map a syntax id to a Nullable
 * @param getKind a function that give the respective Kind of a Token
 */
class ParsingTable[A,Token,Kind] private[ll1compiletime](
    private val entry: Int, 
    private val table: Map[(Int,Kind), SymboleType], 
    private val nt:Map[Int,Any],
    private val nullable: Map[Int,Nullable],
    private val ft:Map[Int,(Any => Any)],
    private val getKind: Token => Kind
){

    /** Context at a point during parsing*/
    private type Context = List[ParsingTableContext]

    /** Short hand for SymboleType */
    private type Instruction = SymboleType

    /**
     * Return a parsing result from a sequence of token
     * 
     * @param tokens sequence of tokens
     * @return the result of parsing the tokens
     */
    def apply(tokens: Iterator[Token]):ParsingResult[A] = {
        parse(entry,List(),tokens).toType[A]
    }

    /**
     * Parse the sequence of Token with given a syntax id and a context
     * 
     * @param s the syntax id
     * @param c the context
     * @param tokens a sequence of token to parse
     * @param tk token of the previous iteration but not used
     */
    @tailrec
    private def parse(s: Int, c: Context, tokens: Iterator[Token], tk:Option[Token] = None): ParsingResult[Any] = {
        val opt:Option[Token] = tk match {
            case Some(_) => tk
            case None => 
                if (tokens.hasNext) then
                    Some(tokens.next)
                else
                    None
        }

        opt match {
            case Some(t) => {
                locate(t,s,c) match {
                    case Some(Right(instr)) => 
                        // token kind matched
                        instr match {
                            case Terminal => 
                                // TOKEN CONSUMED
                                plugValue(t,c) match {
                                    case Left(value) => 
                                        // value and context empty Finished
                                        result(value,tokens.toList)
                                    case Right((s2,c2)) => 
                                        // new found syntax, value saved in new context
                                        parse(s2,c2,tokens)
                            }

                            case NonTerminal(s2, cElem) => parse(s2, cElem::c,tokens,Some(t))
                        }
                    case Some(Left(v)) => 
                        // nullable path taken
                        plugValue(v,c) match {
                            case Left(value) => 
                                // context is empty, parsing finished
                                result(value,tokens.toList)
                            case Right((s2,c2)) => 
                                // new found syntax, value saved in new context
                                parse(s2,c2,tokens,Some(t))
                        }
                    case None => UnexpectedToken(getKind(t),getFirstSetOfSyntax(s)) // no match, not nullable

                }
            }
            case None => 
                nullable.get(s) match {
                    case Some(v) => plugValue(v.get(using nt),c) match {
                        case Left(v) => result(v,Nil)
                        case Right((s2,c2)) => parse(s,c, Iterator.empty)
                    }
                    case None => UnexpectedEnd(getFirstSetOfSyntax(s))

                }
        }
    }

    /**
     * Return a successful parsing result
     */
    private def result(v: Any, tokens: List[Token]) = tokens match {
        case Nil => ParsedSuccessfully(v)
        case _ => ParsedSuccessfullyWithRest(v,tokens)
    }

    /**
     * Return either a produced or the next instruction from a token,
     * a syntax id, and a context
     */
    private def locate(t: Token, s: Int, c: Context): Option[Either[Any, Instruction]] =
        table.get((s,getKind(t))) match {
            case Some(instr) => Some(Right(instr))
            case None => // No Kind match this syntax
                nullable.get(s) match { // so we try to find a nullable value
                    case Some(value) => Some(Left(value.get(using nt)))
                    case None => None

                }
        }


    /**
     * Return the first set of the syntax corresponding to the given id
     * 
     * Used for error printing purpose
     */
    private def getFirstSetOfSyntax(s: Int):Set[Kind] = {
        table.keys.filter(_._1 == s).map(_._2).toSet
    }


    /**
     * Return either the given value when the context is empty
     * or a new (syntax id, context) tuple
     */
    @tailrec
    private def plugValue(v:Any, c:Context): Either[Any,(Int,Context)] = {
        c match {
            case Nil => Left(v)
            case FollowedBy(s)::cs => Right((s,PrependedBy(v)::cs))
            case ApplyF(fId)::cs => plugValue(ft(fId)(v),cs)
            case PrependedBy(vp)::cs => plugValue(new ~(vp,v),cs)
            case PrependedByNullable(s)::cs => plugValue(new ~(nullable(s).get(using nt),v),cs)
        }
    }
}

private[ll1compiletime] object ParsingTable{
    /** Instruction on the next thing to do during parsing. */
    enum SymboleType {
        case NonTerminal(id: Int, elem: ParsingTableContext) extends SymboleType
        case Terminal extends SymboleType
    }

    object SymboleType {
        /** Give an `quoted.Expr` from a SymboleType */
        given SymboleTypeToExpr: ToExpr[SymboleType] with {
            def apply(pti : SymboleType)(using Quotes):Expr[SymboleType] = 
                pti match {
                    case Terminal => '{Terminal}
                    case NonTerminal(i,e) => '{NonTerminal(${Expr(i)}, ${Expr(e)})}
                }
        }
    }

    /** 
     *  The current context at a point during parsing
     * 
     *  (Similar to the Layered Context in [Scallion](https://github.com/epfl-lara/scallion))
     */
    enum ParsingTableContext {
        /** 
         * Apply the function corresponding to the id 
         * 
         * @param id function id
         */
        case ApplyF(id: Int) extends ParsingTableContext
        /** 
         * current syntax in context is prepended by value `v` 
         * 
         * @param v the prepended value
         */
        case PrependedBy(v: Any) extends ParsingTableContext
        /** 
         * current syntax in context is prepended by value corresponding
         * to the id s
         * 
         * @param s the id of the value
         */
        case PrependedByNullable(s: Int) extends ParsingTableContext
        /**
         * The syntax in context is followed by the syntax
         * corresponding to the given id.
         */
        case FollowedBy(s: Int) extends ParsingTableContext
    }

    object ParsingTableContext {
        /** Get `quoted.Expr` from ParsingTableContext */
        given ParsingTableContextToExpr : ToExpr[ParsingTableContext] with {
            def apply(ptc:ParsingTableContext)(using Quotes) = ptc match{
                case ApplyF(f) => '{ApplyF(${Expr(f)})}
                case PrependedBy(_) => throw UnsupportedOperationException(s"This < ${ptc} > should not be used at compile time")
                case PrependedByNullable(s) => '{PrependedByNullable(${Expr(s)})}
                case FollowedBy(s) => '{FollowedBy(${Expr(s)})}
            }
        }
    }

    /** Represent a reference to a nullable value */
    sealed trait Nullable {
        /** 
         * Return the value corresponding to the reference of
         * this `Nullable`
         * 
         * @param table a Map of id to values
         * @return the result corresponding to the reference
         */
        def get(using table: Map[Int,Any]):Any
    }
    object Nullable {
        /** Get `quoted.Expr` from Nullable */
        given ToExpr[Nullable] with {
            def apply(n: Nullable)(using Quotes): Expr[Nullable] = n match {
                case Leaf(i) => '{Leaf(${Expr(i)})}
                case Node(l,r) => '{Node(${Expr(l)},${Expr(r)})}
            }
        }
    }
    /** 
     * Hold a direct reference to a value
     * @param i direct reference to the value
     */
    case class Leaf(i:Int) extends Nullable{
        private var cache: Option[Any] = None
        /**
         * Get the value from that direct reference
         * 
         * @param table a Map from id to values
         */
        def get(using table: Map[Int,Any]):Any = cache match {
            case None => 
                val tmp = table(i).asInstanceOf[Any]
                cache = Some(tmp)
                tmp
            case Some(v) => v
        }
    }
    /**
     * Hold a composed reference to a value
     * @param left the left reference of the composition
     * @param right the left reference of the composition
     */
    case class Node(left: Nullable, right: Nullable) extends Nullable {
        private var cache: Option[Any] = None
        /**
         * Get the value from the composed reference. Compose the value
         * of left with the value of right
         * 
         * @param table a Map from id to values
         */
        def get(using table: Map[Int,Any]):Any = cache match {
            case None => 
                val tmp = (new ~(left.get, right.get)).asInstanceOf[Any]
                cache = Some(tmp)
                tmp
            case Some(v) => v
        }
    }
}