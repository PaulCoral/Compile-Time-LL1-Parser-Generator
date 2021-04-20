package syntax


import scala.annotation._
import TokensAndKinds.Kind
import TokensAndKinds.Token


inline def accept[A](inline k:Kind)(inline f: PartialFunction[Token,A]): Syntax[A] = elem(k).map(f)

inline def epsilon[A](inline e: A): Syntax[A] = Success(e)

inline def elem(inline k: Kind): Syntax[Token] = Elem(k)

transparent inline def recursive[A](inline syntax: Syntax[A]) = Recursive(syntax)

sealed trait Syntax[A]{

  private var id : Option[Long] = None

  def setId(newId: Long):Unit = 
    id match
      case None => id = Some(newId)
      case Some(_) => ()

  def getId: Option[Long] = id

  inline def hasId: Boolean = !id.isEmpty

  /**
   * Disjunction operator
   */
  infix def |(that: Syntax[A]):Syntax[A] = 
    (this, that) match
      case (Failure(), _) => that
      case (_,Failure()) => this
      case _ => Disjunction(this, that)


  /**
   * Sequence operator
   */
  infix def ~[B](that: Syntax[B]):Syntax[(A,B)] = 
    (this, that) match
      case (Failure(),_) => Failure()
      case (_, Failure()) => Failure()
      case _ => Sequence(this, that)

  /**
   * Sequence operator
   */
  infix def ~>~[B](that: Syntax[B]):Syntax[B] = 
    (this, that) match
      case (Failure(),_) => Failure()
      case (_, Failure()) => Failure()
      case _ => Sequence(this, that).map{_._2}

  /**
   * Map this syntax to another
   */
  def map[B](f: A => B): Syntax[B] =
    Transform(this, f)

  
  
}

/**
 * Successful parsing
 */
case class Success[A](value: A) extends Syntax[A]
/**
 * Failed parsing
 */
case class Failure[A]() extends Syntax[A]

/**
 * The parsing of a single Token
 */
case class Elem(e: Kind) extends Syntax[Token]
/**
 * Transformation by applying a function on the result of a successful parsing
 */
case class Transform[A,B](inner: Syntax[A],f : A => B) extends Syntax[B]

/**
 * The parsing of a sequence of Token
 */
case class Sequence[A, B](left: Syntax[A], right: Syntax[B]) extends Syntax[(A, B)]
/**
 * The parsing of a disjunction of Token
 */
case class Disjunction[A](left: Syntax[A], right: Syntax[A]) extends Syntax[A]

/**
 * Recursive construction of a syntaxs
 */
case class Recursive[A](inner : Syntax[A]) extends Syntax[A]


