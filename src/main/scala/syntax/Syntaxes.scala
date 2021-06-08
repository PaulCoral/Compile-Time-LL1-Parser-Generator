package syntax

import scala.annotation.tailrec

import TokensAndKinds._
 

sealed trait Syntax[A](using idc:IdCounter){
  val id = idc.nextId

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
  def accept[A](k:Kind)(f: PartialFunction[Token,A])(using IdCounter): Syntax[A] = elem(k).map(f)

  def epsilon[A](e: A)(using IdCounter): Syntax[A] = Success(e)
  
  def elem(k: Kind)(using IdCounter): Syntax[Token] = Elem(k)

  def recursive[A](syntax: => Syntax[A])(using IdCounter): Syntax[A] = Recursive(syntax)

  import scala.collection.mutable.Set
  def idToFunc(s: Syntax[?], ids:Set[Int] = Set(),acc: Map[Int,(Any => Any)] = Map()):Map[Int, (Any => Any)] = 
    s match {
      case x if ids.contains(x.id) => acc
      case Transform(i,f) => idToFunc(i, ids += s.id,acc + (s.id -> f.asInstanceOf[Any => Any]))
      case Sequence(l,r) =>  idToFunc(r,ids += s.id,idToFunc(l,ids += s.id,acc))
      case Disjunction(l,r) => idToFunc(r,ids += s.id,idToFunc(l,ids += s.id,acc))
      case Recursive(i) => idToFunc(i,ids += s.id,acc)
      case _ => acc
    }
}


/**
 * Successful parsing
 */
case class Success[A](value: A)(using IdCounter) extends Syntax[A]
  

/**
 * Failed parsing
 */
case class Failure[A]()(using IdCounter) extends Syntax[A]
  

/**
 * The parsing of a single Token
 */
case class Elem(e: Kind)(using IdCounter) extends Syntax[Token]
  

/**
 * Transformation by applying a function on the result of a successful parsing
 */
case class Transform[A,B](inner: Syntax[A],f : A => B)(using IdCounter) extends Syntax[B]
  

/**
 * The parsing of a sequence of Token
 */
case class Sequence[A, B](left: Syntax[A], right: Syntax[B])(using IdCounter) extends Syntax[(A, B)]


/**
 * The parsing of a disjunction of Token
 */
case class Disjunction[A](left: Syntax[A], right: Syntax[A])(using IdCounter) extends Syntax[A]
  

/**
 * Recursive construction of a syntaxs
 */
class Recursive[A](syntax: => Syntax[A])(using IdCounter) extends Syntax[A]{
  lazy val inner = syntax
  override def toString = s"<Recursive_id:$id>"
}

object Recursive {
  def apply[A](syntax: => Syntax[A])(using IdCounter): Recursive[A] = new Recursive(syntax)

  def unapply[A](that: Syntax[A]): Option[Syntax[A]] = {
    if (that.isInstanceOf[Recursive[_]]) {
      val other = that.asInstanceOf[Recursive[A]]
      Some(other.inner)
    } else {
      None
    }
  }
}


  
