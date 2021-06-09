package ll1compiletime.syntax

import scala.annotation.tailrec
 

sealed trait Syntax[A,Token,Kind](using idc:IdCounter){
  val id = idc.nextId

  /**
   * Disjunction operator
   */
  infix def |(that: Syntax[A,Token,Kind]):Syntax[A,Token,Kind] = 
    (this, that) match
      case (Failure(), _) => that
      case (_,Failure()) => this
      case _ => Disjunction(this, that)


  /**
   * Sequence operator
   */
  infix def ~[B](that: Syntax[B,Token,Kind]):Syntax[(A,B),Token,Kind] = 
    (this, that) match
      case (Failure(),_) => Failure()
      case (_, Failure()) => Failure()
      case _ => Sequence(this, that)

  /**
   * Sequence operator, keeping the left value
   */
  infix def ~<~[B](that: Syntax[B,Token,Kind]):Syntax[A,Token,Kind] = 
    (this, that) match
      case (Failure(),_) => Failure()
      case (_, Failure()) => Failure()
      case _ => Sequence(this, that).map{_._1}

  /**
   * Sequence operator, keeping the right value
   */
  infix def ~>~[B](that: Syntax[B,Token,Kind]):Syntax[B,Token,Kind] = 
    (this, that) match
      case (Failure(),_) => Failure()
      case (_, Failure()) => Failure()
      case _ => Sequence(this, that).map{_._2}

  /**
   * Map this syntax to another
   */
  def map[B](f: A => B): Syntax[B,Token,Kind] =
    Transform(this, f)
}

object Syntax {
  def accept[A,Token,Kind](k:Kind)(f: PartialFunction[Token,A])(using IdCounter): Syntax[A,Token,Kind] = elem(k).map(f)

  def epsilon[A,Token,Kind](e: A)(using IdCounter): Syntax[A,Token,Kind] = Success[A,Token,Kind](e)
  
  def elem[Kind,Token](k: Kind)(using IdCounter): Syntax[Token,Token,Kind] = Elem(k)

  def recursive[A,Token,Kind](syntax: => Syntax[A,Token,Kind])(using IdCounter): Syntax[A,Token,Kind] = Recursive(syntax)

  import scala.collection.mutable.Set
  def idToFunc[Token,Kind](s: Syntax[?,Token,Kind], ids:Set[Int] = Set(),acc: Map[Int,(Any => Any)] = Map()):Map[Int, (Any => Any)] = 
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
case class Success[A,Token,Kind](value: A)(using IdCounter) extends Syntax[A,Token,Kind]
  

/**
 * Failed parsing
 */
case class Failure[A,Token,Kind]()(using IdCounter) extends Syntax[A,Token,Kind]
  

/**
 * The parsing of a single Token
 */
case class Elem[Token,Kind](e: Kind)(using IdCounter) extends Syntax[Token,Token,Kind]
  

/**
 * Transformation by applying a function on the result of a successful parsing
 */
case class Transform[A,B,Token,Kind](inner: Syntax[A,Token,Kind],f : A => B)(using IdCounter) extends Syntax[B,Token,Kind]
  

/**
 * The parsing of a sequence of Token
 */
case class Sequence[A, B,Token,Kind](left: Syntax[A,Token,Kind], right: Syntax[B,Token,Kind])(using IdCounter) extends Syntax[(A, B),Token,Kind]


/**
 * The parsing of a disjunction of Token
 */
case class Disjunction[A,Token,Kind](left: Syntax[A,Token,Kind], right: Syntax[A,Token,Kind])(using IdCounter) extends Syntax[A,Token,Kind]
  

/**
 * Recursive construction of a syntaxs
 */
class Recursive[A,Token,Kind](syntax: => Syntax[A,Token,Kind])(using IdCounter) extends Syntax[A,Token,Kind]{
  lazy val inner = syntax
  override def toString = s"<Recursive_id:$id>"
}

object Recursive {
  def apply[A,Token,Kind](syntax: => Syntax[A,Token,Kind])(using IdCounter): Recursive[A,Token,Kind] = new Recursive(syntax)

  def unapply[A,Token,Kind](that: Syntax[A,Token,Kind]): Option[Syntax[A,Token,Kind]] = {
    if (that.isInstanceOf[Recursive[?,Token,Kind]]) {
      val other = that.asInstanceOf[Recursive[A,Token,Kind]]
      Some(other.inner)
    } else {
      None
    }
  }
}


  
