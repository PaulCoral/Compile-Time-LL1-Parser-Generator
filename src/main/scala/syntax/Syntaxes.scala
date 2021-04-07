package syntax

import TokensAndKinds._

object Syntaxes:

  def epsilon[A](e: A) = Success(e)
  
  def elem(t: Token) = Elem(getKind(t))

  sealed trait Syntax[A]:
    /**
     * If the syntax is nullable
     */
    lazy val isNullable: Boolean

    /**
     * The first set of this syntax
     */
    lazy val first: Set[Kind]

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
  

  case class Elem(e: Kind) extends Syntax[Token]:
    lazy val isNullable = false

    lazy val first = Set(e)

    lazy val snf = Set()

    lazy val hasConflict = false


  case class Transform[A,B](inner: Syntax[A],f : A => B) extends Syntax[B]:
    lazy val isNullable = inner.isNullable

    lazy val first = inner.first

    lazy val snf = inner.snf

    lazy val hasConflict = inner.hasConflict
  

  case class Sequence[A, B](left: Syntax[A], right: Syntax[B]) extends Syntax[(A, B)]:
    lazy val isNullable = left.isNullable && right.isNullable

    lazy val first = (if(right.isProductive) then left.first else Set()) ++ (if(left.isNullable) then right.first else Set())

    lazy val snf = 
      (if(right.isNullable) then left.snf else Set()) ++
      (if(left.isProductive) then right.snf else Set())

    lazy val hasConflict = 
      left.hasConflict  ||
      right.hasConflict ||
      snf.intersect(first).nonEmpty
  

  case class Disjunction[A](left: Syntax[A], right: Syntax[A]) extends Syntax[A]:
    lazy val isNullable = left.isNullable || right.isNullable

    lazy val first = left.first ++ right.first

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
    lazy val isNullable = ???

    lazy val first = ???

    lazy val snf = ???

    lazy val hasConflict = ???
    
    val id = Recursive.nextId

    lazy val inner = syntax


  object Recursive:
    private var id: Int = 0

    def nextId = 
      val tmp = id
      id += 1
      tmp

    def apply[A](syntax: => Syntax[A]) = new Recursive(syntax)


  
