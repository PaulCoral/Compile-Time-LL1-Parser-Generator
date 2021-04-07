package parser

import syntax.Syntaxes._
import syntax.TokensAndKinds._

case class Parser[A](syntax: Syntax[A]):
  def apply(ts : Seq[Token]) = ???

object Parser:
  def apply[A](syntax: Syntax[A]): Parser[A] = new Parser(syntax)

  def first[A](syntax: Syntax[A]) : Set[Kind] = syntax match
    case Success(_) => Set()
    case Failure() => Set()
    case Elem(k) => Set(k)
    case Transform(i,_) => first(i)
    case Sequence(l,r) => first(l)

  def snf : Set[Kind] = ???

  def hasConflict[A](syntax: Syntax[A]): Boolean = ???
    

  