package syntax


import scala.annotation._
import TokensAndKinds.Kind
import TokensAndKinds.Token


def accept[A](k:Kind)(f: PartialFunction[Token,A]): Syntax[A] = elem(k).map(f)

def epsilon[A](e: A): Syntax[A] = Success(e)

def elem(k: Kind): Syntax[Token] = Elem(k)

def recursive[A](syntax: => Syntax[A]): Syntax[A] = Recursive(syntax)

sealed trait Syntax[A]{

  val id : Long

  /**
   * Disjunction operator
   */
  infix def |(that: Syntax[A]):Syntax[A] = 
    (this, that) match
      case (Failure(_), _) => that
      case (_,Failure(_)) => this
      case _ => Disjunction(this, that)


  /**
   * Sequence operator
   */
  infix def ~[B](that: Syntax[B]):Syntax[(A,B)] = 
    (this, that) match
      case (Failure(_),_) => Failure()
      case (_, Failure(_)) => Failure()
      case _ => Sequence(this, that)

  /**
   * Sequence operator
   */
  infix def ~>~[B](that: Syntax[B]):Syntax[B] = 
    (this, that) match
      case (Failure(_),_) => Failure()
      case (_, Failure(_)) => Failure()
      case _ => Sequence(this, that).map{_._2}

  /**
   * Map this syntax to another
   */
  def map[B](f: A => B): Syntax[B] =
    Transform(this, f)

  
  
}

object Syntax{
  private var id: Long = 0

  inline def getNextId : Long = {
    val prev = id
    id = id + 1
    prev
  }
}

/**
 * Successful parsing
 */
case class Success[A](value: A, id: Long = Syntax.getNextId) extends Syntax[A]
/**
 * Failed parsing
 */
case class Failure[A](id: Long = Syntax.getNextId) extends Syntax[A]

/**
 * The parsing of a single Token
 */
case class Elem(e: Kind, id: Long = Syntax.getNextId) extends Syntax[Token]
/**
 * Transformation by applying a function on the result of a successful parsing
 */
case class Transform[A,B](inner: Syntax[A],f : A => B, id: Long = Syntax.getNextId) extends Syntax[B]

/**
 * The parsing of a sequence of Token
 */
case class Sequence[A, B](left: Syntax[A], right: Syntax[B], id: Long = Syntax.getNextId) extends Syntax[(A, B)]
/**
 * The parsing of a disjunction of Token
 */
case class Disjunction[A](left: Syntax[A], right: Syntax[A], id: Long = Syntax.getNextId) extends Syntax[A]

/**
 * Recursive construction of a syntaxs
 */
case class Recursive[A](inner : Syntax[A], id: Long = Syntax.getNextId) extends Syntax[A]


