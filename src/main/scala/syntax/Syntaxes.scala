package syntax


import TokensAndKinds._
 

sealed trait Syntax[A]{
  val id = Syntax.nextId

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
   * Sequence operator, keeping the left value
   */
  infix def ~<~[B](that: Syntax[B]):Syntax[A] = 
    (this, that) match
      case (Failure(),_) => Failure()
      case (_, Failure()) => Failure()
      case _ => Sequence(this, that).map{_._1}

  /**
   * Sequence operator, keeping the right value
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

object Syntax {
  private var id : Int = 0

  def nextId = {
    val prev: Int = id
    id = id + 1
    prev
  }

  def accept[A](k:Kind)(f: PartialFunction[Token,A]): Syntax[A] = elem(k).map(f)

  def epsilon[A](e: A): Syntax[A] = Success(e)
  
  def elem(k: Kind): Syntax[Token] = Elem(k)

  def recursive[A](syntax: => Syntax[A]): Syntax[A] = Recursive(syntax)

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
class Recursive[A](syntax: => Syntax[A]) extends Syntax[A]{
  lazy val inner = syntax
  override def toString = "<Recursive>"
}

object Recursive {
  def apply[A](syntax: => Syntax[A]): Recursive[A] = new Recursive(syntax)

  def unapply[A](rec: Recursive[A]): Option[Syntax[A]] =
    if rec.isInstanceOf[Recursive[A]] then
      Some(rec.inner)
    else
      None
}


  
