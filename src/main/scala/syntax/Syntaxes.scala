package syntax


import scala.annotation._

trait Syntaxes:

  type RecUID = Long

  type Token

  type Kind

  def getKind(t: Token): Kind

  def accept[A](k:Kind)(f: PartialFunction[Token,A]): Syntax[A] = elem(k).map(f)

  def epsilon[A](e: A): Syntax[A] = Success(e)
  
  def elem(k: Kind): Syntax[Token] = Elem(k)

  def recursive[A](syntax: => Syntax[A]): Syntax[A] = Recursive(syntax)

  sealed trait Syntax[A]:

    /**
     * If the syntax is nullable
     */
    lazy val nullable: Option[A]

    
    lazy val isNullable: Boolean = nullable.nonEmpty


    lazy val first: Set[Kind] = computeFirst()

    /**
     * The first set of this syntax
     */

    def computeFirst(visited: Set[RecUID] = Set(), acc: Set[Kind] = Set()):Set[Kind]

    /**
     * The Should-Not-Follow set of this syntax 
     */
    lazy val snf: Set[Kind]

    /**
     * If this syntax has conflict 
     */
    lazy val hasConflict: Boolean

    /**
     * If this syntax is productive
     */
    lazy val isProductive: Boolean

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
     * Map this syntax to another
     */
    def map[B](f: A => B): Syntax[B] =
      Transform(this, f)

  /**
   * Successful parsing
   */
  case class Success[A](value: A) extends Syntax[A]:
    lazy val nullable: Option[A] = Some(value)

    def computeFirst(visited: Set[RecUID], acc: Set[Kind] = Set()) = acc

    lazy val snf: Set[Kind] = Set()

    lazy val hasConflict: Boolean = false

    lazy val isProductive: Boolean = true
  

  /**
   * Failed parsing
   */
  case class Failure[A]() extends Syntax[A]:
    lazy val nullable: Option[A] = None

    def computeFirst(visited: Set[RecUID], acc: Set[Kind] = Set()) = acc

    lazy val snf: Set[Kind] = Set()

    lazy val hasConflict:Boolean = false

    lazy val isProductive: Boolean = false
  

  /**
   * The parsing of a single Token
   */
  case class Elem(e: Kind) extends Syntax[Token]:
    lazy val nullable: Option[Token] = None

    def computeFirst(visited: Set[RecUID], acc: Set[Kind] = Set()) = acc + e

    lazy val snf: Set[Kind] = Set()

    lazy val hasConflict:Boolean = false
  
    lazy val isProductive: Boolean = true


  /**
   * Transformation by applying a function on the result of a successful parsing
   */
  case class Transform[A,B](inner: Syntax[A],f : A => B) extends Syntax[B]:
    lazy val nullable: Option[B] = inner.nullable.map(f)

    def computeFirst(visited: Set[RecUID], acc: Set[Kind] = Set()) = 
      inner.computeFirst(visited, acc)

    lazy val snf: Set[Kind] = inner.snf

    lazy val hasConflict:Boolean = inner.hasConflict

    lazy val isProductive: Boolean = inner.isProductive
  

  /**
   * The parsing of a sequence of Token
   */
  case class Sequence[A, B](left: Syntax[A], right: Syntax[B]) extends Syntax[(A, B)]:
    lazy val nullable: Option[(A,B)] = left.nullable.flatMap( ln => right.nullable.map(rn => (ln, rn)) )

    def computeFirst(visited: Set[RecUID], acc: Set[Kind] = Set()) =
      (if(right.isProductive) then left.computeFirst(visited, acc) else Set()) 
      ++ (if(left.isNullable) then right.computeFirst(visited, acc) else Set())

    lazy val snf: Set[Kind] = 
      (if(right.isNullable) then left.snf else Set()) ++
      (if(left.isProductive) then right.snf else Set())

    lazy val hasConflict:Boolean = 
      left.hasConflict  ||
      right.hasConflict ||
      snf.intersect(first).nonEmpty

    lazy val isProductive: Boolean = left.isProductive && right.isProductive
  

  /**
   * The parsing of a disjunction of Token
   */
  case class Disjunction[A](left: Syntax[A], right: Syntax[A]) extends Syntax[A]:
    lazy val nullable: Option[A] = left.nullable.orElse(right.nullable)

    def computeFirst(visited: Set[RecUID], acc: Set[Kind] = Set()) =
      left.computeFirst(visited, right.computeFirst(visited,acc))

    lazy val snf: Set[Kind] =
      left.snf ++
      right.snf ++
      (if(left.isNullable) then right.first else Set()) ++
      (if(right.isNullable) then left.first else Set())

    lazy val hasConflict = 
      (left.isNullable && right.isNullable) ||
      (left.first.intersect(right.first).nonEmpty) ||
      left.hasConflict ||
      right.hasConflict

    lazy val isProductive: Boolean = left.isProductive || right.isProductive


  /**
   * Recursive construction of a syntaxs
   */
  class Recursive[A](syntax: => Syntax[A], val uid: RecUID) extends Syntax[A]:

    lazy val nullable: Option[A] = inner.nullable

    def computeFirst(visited: Set[RecUID], acc: Set[Kind] = Set()) =
      inner.computeFirst(visited + uid, acc)

    lazy val snf: Set[Kind] = inner.snf

    lazy val hasConflict:Boolean = inner.hasConflict
    
    val id = Recursive.nextId

    lazy val inner: Syntax[A] = syntax

    lazy val isProductive: Boolean = inner.isProductive


  object Recursive:
    private var id: RecUID = 0

    private def nextId: RecUID = 
      val tmp = id
      id += 1
      tmp

    def apply[A](syntax: => Syntax[A]) = new Recursive(syntax, nextId)

    def unapply[A](that: Syntax[A]): Option[(Syntax[A], RecUID)] = 
      if(that.isInstanceOf[Recursive[_]])
        val asRec = that.asInstanceOf[Recursive[A]]
        Some((asRec.inner,asRec.uid))
      else
        None
      


  
