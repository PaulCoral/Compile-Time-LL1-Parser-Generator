package ll1compiletime.syntax

import ll1compiletime.~
 

private[ll1compiletime] sealed trait Syntax[A,Token,Kind](using idc:IdCounter){
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

  /**
   * define an optional syntax
   */
  private[ll1compiletime] def opt: Syntax[Option[A],Token,Kind] =
    this.map(Some(_)) | (Syntax.epsilon(None))
}

private[ll1compiletime] object Syntax {
  def accept[A,Token,Kind](k:Kind)(f: PartialFunction[Token,A])(using IdCounter): Syntax[A,Token,Kind] = elem(k).map(f)

  def epsilon[A,Token,Kind](e: A)(using IdCounter): Syntax[A,Token,Kind] = Success[A,Token,Kind](e)

  def failure[A,Token,Kind](using IdCounter): Syntax[A,Token,Kind] = Failure()
  
  def elem[Token,Kind](k: Kind)(using IdCounter): Syntax[Token,Token,Kind] = Elem(k)

  def recursive[A,Token,Kind](syntax: => Syntax[A,Token,Kind])(using IdCounter): Syntax[A,Token,Kind] = Recursive(syntax)

  /**
   * Collect the inner data of syntax which can't be used
   * at compile time
   */
  def runtimeSyntaxData[Token,Kind](
    syntax: Syntax[?,Token,Kind]
  ):(Map[Int, (Any => Any)],Map[Int, Any]) = {
    import scala.collection.mutable.{Set,Map,Queue}

    // id -> transform func
    val ft = Map[Int, (Any => Any)]()
    // id -> base value
    val nt = Map[Int, Any]()
    // visited ids
    val visited = Set[Int]()
    // the queue of syntax to be processed
    val queue = Queue(syntax)

    while(!(queue.isEmpty)) {
      val s = queue.dequeue
      if(!(visited.contains(s.id))) {
        visited += s.id
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
    (ft.toMap,nt.toMap) // as immutable Maps
  }
}




/**
 * Syntax for an empty sequence of token
 * 
 * @tparam A the type of the resulting parsing value
 * @tparam Token the Token type used in parsing
 * @tparam Kind the Kind of the Tokens used in parsing
 * @param value the value produced on an empty sequence of token
 */
private[ll1compiletime] case class Success[A,Token,Kind](value: A)(using IdCounter) extends Syntax[A,Token,Kind]
  

/**
 * Empty Syntax, result in parsing failure
 * 
 * @tparam A the type of the resulting parsing value
 * @tparam Token the Token type used in parsing
 * @tparam Kind the Kind of the Tokens used in parsing
 */
private[ll1compiletime] case class Failure[A,Token,Kind]()(using IdCounter) extends Syntax[A,Token,Kind]
  

/**
 * Syntax for parsing a single Token of the given Kind.
 * Produce the Token as parsing value
 * 
 * @tparam Token the Token type used in parsing
 * @tparam Kind the Kind of the Tokens used in parsing
 * @param e the Kind of the Token to parse
 */
private[ll1compiletime] case class Elem[Token,Kind](e: Kind)(using IdCounter) extends Syntax[Token,Token,Kind]
  

/**
 * Transformation of a Syntax by applying a function on the result of a successful parsing
 * of the inner syntax
 * 
 * @tparam A type of the the parsed value before the tranformation
 * @tparam B type of the the parsed value after the tranformation
 * @param inner the inner syntax
 * @param f the function to apply to the parsed value
 */
private[ll1compiletime] case class Transform[A,B,Token,Kind](inner: Syntax[A,Token,Kind],f : A => B)(using IdCounter) extends Syntax[B,Token,Kind]
  

/**
 * Syntax of a sequence
 * 
 * @param left the first syntax in the sequence
 * @param right the second syntax in the sequence
 */
private[ll1compiletime] case class Sequence[A, B,Token,Kind](left: Syntax[A,Token,Kind], right: Syntax[B,Token,Kind])(using IdCounter) extends Syntax[A ~ B,Token,Kind]


/**
 * Syntax of a disjunction
 * 
 * @param left one of the syntax
 * @param left the other syntax
 */
private[ll1compiletime] case class Disjunction[A,Token,Kind](left: Syntax[A,Token,Kind], right: Syntax[A,Token,Kind])(using IdCounter) extends Syntax[A,Token,Kind]
  

/**
 * Recursive construction of a syntax
 * 
 * @param syntax the inner syntax to define as recursive
 */
private[ll1compiletime] class Recursive[A,Token,Kind](syntax: => Syntax[A,Token,Kind])(using IdCounter) extends Syntax[A,Token,Kind]{
  lazy val inner = syntax
  override def toString = s"<Recursive_id:$id>"
}

private[ll1compiletime] object Recursive {

  /**
   * Construct a recursive syntax form the inner syntax
   * 
   * @param syntax the inner syntax
   * @return a recursive inner syntax
   */
  def apply[A,Token,Kind](syntax: => Syntax[A,Token,Kind])(using IdCounter): Recursive[A,Token,Kind] = new Recursive(syntax)

  /** Extract the inner syntax */
  def unapply[A,Token,Kind](that: Syntax[A,Token,Kind]): Option[Syntax[A,Token,Kind]] = {
    if (that.isInstanceOf[Recursive[?,Token,Kind]]) {
      val other = that.asInstanceOf[Recursive[A,Token,Kind]]
      Some(other.inner)
    } else {
      None
    }
  }
}


  
