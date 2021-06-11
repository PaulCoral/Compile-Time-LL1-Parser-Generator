package ll1compiletime.syntax

import scala.annotation.tailrec
 

sealed trait Syntax[A,Token,Kind](using idc:IdCounter){
  val id = idc.nextId

  /**
   * Disjunction operator
   */
  private[ll1compiletime] infix def |(that: Syntax[A,Token,Kind]):Syntax[A,Token,Kind] = 
    (this, that) match
      case (Failure(), _) => that
      case (_,Failure()) => this
      case _ => Disjunction(this, that)


  /**
   * Sequence operator
   */
  private[ll1compiletime] infix def ~[B](that: Syntax[B,Token,Kind]):Syntax[A ~ B,Token,Kind] = 
    (this, that) match
      case (Failure(),_) => Failure()
      case (_, Failure()) => Failure()
      case (Success(a),Success(b)) => Success(new ~(a,b))
      case _ => Sequence(this, that)

  /**
   * Sequence operator, keeping the left value
   */
  private[ll1compiletime] infix def ~<~[B](that: Syntax[B,Token,Kind]):Syntax[A,Token,Kind] = 
    (this, that) match
      case (Failure(),_) => Failure()
      case (_, Failure()) => Failure()
      case _ => Sequence(this, that).map{_._1}

  /**
   * Sequence operator, keeping the right value
   */
  private[ll1compiletime] infix def ~>~[B](that: Syntax[B,Token,Kind]):Syntax[B,Token,Kind] = 
    (this, that) match
      case (Failure(),_) => Failure()
      case (_, Failure()) => Failure()
      case _ => Sequence(this, that).map{_._2}

  /**
   * Map this syntax to another
   */
  private[ll1compiletime] def map[B](f: A => B): Syntax[B,Token,Kind] =
    Transform(this, f)


  private[ll1compiletime] def opt: Syntax[Option[A],Token,Kind] =
    this.map(Some(_)) | (Syntax.epsilon(None))
}

object Syntax {
  private[ll1compiletime] def accept[A,Token,Kind](k:Kind)(f: PartialFunction[Token,A])(using IdCounter): Syntax[A,Token,Kind] = elem(k).map(f)

  private[ll1compiletime] def epsilon[A,Token,Kind](e: A)(using IdCounter): Syntax[A,Token,Kind] = Success[A,Token,Kind](e)

  private[ll1compiletime] def failure[A,Token,Kind](using IdCounter): Syntax[A,Token,Kind] = Failure()
  
  private[ll1compiletime] def elem[Token,Kind](k: Kind)(using IdCounter): Syntax[Token,Token,Kind] = Elem(k)

  private[ll1compiletime] def recursive[A,Token,Kind](syntax: => Syntax[A,Token,Kind])(using IdCounter): Syntax[A,Token,Kind] = Recursive(syntax)

  def runtimeSyntaxData[Token,Kind](
    syntax: Syntax[?,Token,Kind]
  ):(Map[Int, (Any => Any)],Map[Int, Any]) = {
    import scala.collection.mutable.{Set,Map,Queue}

    val ft = Map[Int, (Any => Any)]()
    val nt = Map[Int, Any]()
    val ids = Set[Int]()
    val queue = Queue(syntax)

    while(!(queue.isEmpty)) {
      val s = queue.dequeue
      if(!(ids.contains(s.id))) {
        ids += s.id
        s match {
          case Transform(i,f) => {
            ft += (s.id -> f.asInstanceOf[Any => Any])
            queue.enqueue(i)
          }
          case Sequence(l,r) => queue.enqueue(l,r)
          case Disjunction(l,r) => queue.enqueue(l,r)
          case Recursive(i) => queue.enqueue(i)
          case Success(v) => nt += (s.id -> v.asInstanceOf[Any])
          case _ => ()
        }
      }
    }
    (ft.toMap,nt.toMap) // as immutable Map
  }
}

case class ~[+A, +B](_1: A, _2: B) {
    /* Builds a pair. */
    def ~[C](next: C): (A ~ B) ~ C = new ~(this, next)
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
case class Sequence[A, B,Token,Kind](left: Syntax[A,Token,Kind], right: Syntax[B,Token,Kind])(using IdCounter) extends Syntax[A ~ B,Token,Kind]


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


  
