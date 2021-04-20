package syntax

/*
  AST minimal: Epsilon, Failure, Elem, Sequence, Disjunction, (Transform ?), et Recursive.
  lazy val manyAs: Syntax[Unit] = recursive { epsilon(()) | elem(kindA) ~>~ manyAs }
 */

object TokensAndKinds:

  enum Token:
    case IntLitToken(value: Int)
    case IdentifierToken(id : String)
  
  enum Kind(val id : Long):
    case IntKind extends Kind(Kind.nextId)
    case IdentifierKind extends Kind(Kind.nextId)
  object Kind:
    var id : Long = 0
    inline def nextId = 
      val prec = id
      id = id + 1
      prec

  
  def getKind(t: Token): Kind = 
    import Token._
    import Kind._
    t match
      case IntLitToken(_) => IntKind
      case IdentifierToken(_) => IdentifierKind
  
