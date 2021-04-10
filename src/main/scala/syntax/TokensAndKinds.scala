package syntax

/*
  AST minimal: Epsilon, Failure, Elem, Sequence, Disjunction, (Transform ?), et Recursive.
  lazy val manyAs: Syntax[Unit] = recursive { epsilon(()) | elem(kindA) ~>~ manyAs }
 */

object TokensAndKinds:

  enum Token:
    case IntLitToken(value: Int)
    case IdentifierToken(id : String)
  
  enum Kind:
    case IntKind
    case IdentifierKind

  
  def getKind(t: Token): Kind = 
    import Token._
    import Kind._
    t match
      case IntLitToken(_) => IntKind
      case IdentifierToken(_) => IdentifierKind
  
