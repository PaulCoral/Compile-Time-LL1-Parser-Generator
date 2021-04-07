package syntax

import TokensAndKinds._

object Syntaxes:

  sealed trait Syntax[A]:
    lazy val isNullable: Boolean

    lazy val first: Set[Kind]

    lazy val snf: Set[Kind]

    lazy val hasConflict: Boolean

    def |(that: Syntax[A]):Syntax[A] = 
      (this, that) match
        case (Failure(), _) => that
        case (_,Failure()) => this
        case _ => Disjunction(this, that)
  
    def ~[B](that: Syntax[B]):Syntax[(A,B)] = 
      (this, that) match
        case (Failure(),_) => Failure()
        case (_, Failure()) => Failure()
        case _ => Sequence(this, that)
  
  case class Success[A](value: A) extends Syntax[A]:
    lazy val isNullable = true
    lazy val first = Set()
    lazy val snf = Set()
    lazy val hasConflict = false
  
  case class Failure[A]() extends Syntax[A]:
    lazy val isNullable = true
    lazy val first = Set()
    lazy val snf = Set()
    lazy val hasConflict = false
  
  case class Elem(e: Kind) extends Syntax[Token]

  case class Transform[A,B](inner: Syntax[A],f : A => B) extends Syntax[B]
  
  case class Sequence[A, B](left: Syntax[A], right: Syntax[B]) extends Syntax[(A, B)]
  
  case class Disjunction[A](left: Syntax[A], right: Syntax[A]) extends Syntax[A]

  class Recursive[A](syntax: => Syntax[A]) extends Syntax[A]:
    val id = Recursive.nextId
    lazy val inner = syntax

  object Recursive:
    private var id: Int = 0

    def nextId = 
      val tmp = id
      id += 1
      tmp

    def apply[A](syntax: => Syntax[A]) = new Recursive(syntax)

  
  def epsilon[A](e: A) = Success(e)
  
  def elem(t: Token) = Elem(getKind(t))
  
  
