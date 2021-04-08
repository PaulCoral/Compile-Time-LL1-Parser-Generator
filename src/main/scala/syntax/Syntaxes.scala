package syntax


import scala.annotation._

trait Syntaxes:

  type RecUID = Long

  type Token

  type Kind

  def getKind(t: Token): Kind

  def accept[A](k:Kind)(f: PartialFunction[Token,A]) = elem(k).map(f)

  def epsilon[A](e: A) = Success(e)
  
  def elem(k: Kind) = Elem(k)

  sealed trait Syntax[A]:

    /**
     * If the syntax is nullable
     */
    lazy val nullable: Option[A]

    
    lazy val isNullable = nullable.nonEmpty


    lazy val first = computeFirst()

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
    lazy val isProductive: Boolean = isNullable || first.nonEmpty

    /**
     * Disjunction operator
     */
    def |(that: Syntax[A]):Syntax[A] = 
      (this, that) match
        case (Failure(), _) => that
        case (_,Failure()) => this
        case _ => Disjunction(this, that)
  

    /**
     * Sequence operator
     */
    def ~[B](that: Syntax[B]):Syntax[(A,B)] = 
      (this, that) match
        case (Failure(),_) => Failure()
        case (_, Failure()) => Failure()
        case _ => Sequence(this, that)

    /**
     * Map this syntax to another
     */
    def map[B](f: A => B): Syntax[B] =
      Transform(this, f)


  case class Success[A](value: A) extends Syntax[A]:
    lazy val nullable = Some(value)

    def computeFirst(visited: Set[RecUID], acc: Set[Kind] = Set()) = acc

    lazy val snf = Set()

    lazy val hasConflict = false
  

  case class Failure[A]() extends Syntax[A]:
    lazy val nullable = None

    def computeFirst(visited: Set[RecUID], acc: Set[Kind] = Set()) = acc

    lazy val snf = Set()

    lazy val hasConflict = false
  

  case class Elem(e: Kind) extends Syntax[Token]:
    lazy val nullable = None

    def computeFirst(visited: Set[RecUID], acc: Set[Kind] = Set()) = acc + e

    lazy val snf = Set()

    lazy val hasConflict = false


  case class Transform[A,B](inner: Syntax[A],f : A => B) extends Syntax[B]:
    lazy val nullable = inner.nullable.map(f)

    def computeFirst(visited: Set[RecUID], acc: Set[Kind] = Set()) = 
      inner.computeFirst(visited, acc)

    lazy val snf = inner.snf

    lazy val hasConflict = inner.hasConflict
  

  case class Sequence[A, B](left: Syntax[A], right: Syntax[B]) extends Syntax[(A, B)]:
    lazy val nullable = left.nullable.flatMap( ln => right.nullable.map(rn => (ln, rn)) )

    def computeFirst(visited: Set[RecUID], acc: Set[Kind] = Set()) =
      (if(right.isProductive) then left.first else Set()) ++ (if(left.isNullable) then right.first else Set())

    lazy val snf = 
      (if(right.isNullable) then left.snf else Set()) ++
      (if(left.isProductive) then right.snf else Set())

    lazy val hasConflict = 
      left.hasConflict  ||
      right.hasConflict ||
      snf.intersect(first).nonEmpty
  

  case class Disjunction[A](left: Syntax[A], right: Syntax[A]) extends Syntax[A]:
    lazy val nullable = left.nullable.orElse(right.nullable)

    def computeFirst(visited: Set[RecUID], acc: Set[Kind] = Set()) =
      left.computeFirst(visited, right.computeFirst(visited,acc))

    lazy val snf =
      left.snf ++
      right.snf ++
      (if(left.isNullable) then right.first else Set()) ++
      (if(right.isNullable) then left.first else Set())

    lazy val hasConflict = 
      (left.isNullable && right.isNullable) ||
      (left.first.intersect(right.first).nonEmpty) ||
      left.hasConflict ||
      right.hasConflict


  class Recursive[A](syntax: => Syntax[A]) extends Syntax[A]:

    val uid = Recursive.nextId

    lazy val nullable = inner.nullable

    def computeFirst(visited: Set[RecUID], acc: Set[Kind] = Set()) =
      inner.computeFirst(visited + uid, acc)

    lazy val snf = inner.snf

    lazy val hasConflict = inner.hasConflict
    
    val id = Recursive.nextId

    lazy val inner = syntax


  object Recursive:
    private var id: Int = 0

    def nextId = 
      val tmp = id
      id += 1
      tmp

    def apply[A](syntax: => Syntax[A]) = new Recursive(syntax)


  
