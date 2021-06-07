package syntax

/*
  AST minimal: Epsilon, Failure, Elem, Sequence, Disjunction, (Transform ?), et Recursive.
  lazy val manyAs: Syntax[Unit] = recursive { epsilon(()) | elem(kindA) ~>~ manyAs }
 */

object TokensAndKinds {

  enum Token {
    case IntLitToken(value: Int) extends Token
    case IdentifierToken(id : String) extends Token

    def toKind: Kind = {
      import Token._
      import Kind._
      this match {
        case IntLitToken(_) => IntKind
        case IdentifierToken(_) => IdentifierKind
      }
    }
  }
  
  enum Kind {
    case IntKind extends Kind
    case IdentifierKind extends Kind
  }

  object Kind {
    import scala.quoted._
    given KindToExpr : ToExpr[Kind] with {
      def apply(k:Kind)(using Quotes): Expr[Kind] = k match {
        case IntKind => '{IntKind}
        case IdentifierKind => '{IdentifierKind}
      }
    }
  }
}