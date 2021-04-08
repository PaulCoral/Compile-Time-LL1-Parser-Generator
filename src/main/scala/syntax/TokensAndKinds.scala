package syntax

/*
  AST minimal: Epsilon, Failure, Elem, Sequence, Disjunction, (Transform ?), et Recursive.
  lazy val manyAs: Syntax[Unit] = recursive { epsilon(()) | elem(kindA) ~>~ manyAs }
 */

object TokensAndKinds:
  sealed trait Token
  
  case class IntLitToken(value: Int) extends Token
  
  sealed trait Kind
  
  case object IntKind extends Kind

  
  def getKind(t: Token): Kind = t match
    case IntLitToken(_) => IntKind
  
