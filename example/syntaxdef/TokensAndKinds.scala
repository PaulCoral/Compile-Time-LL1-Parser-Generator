package example.syntaxdef


enum MyToken {
  case IntLitToken(value: Int) extends MyToken
  case IdentifierToken(id : String) extends MyToken
}

object MyToken {
    def getKind(t:MyToken): MyKind = {
      import MyToken._
      import MyKind._
      t match {
        case IntLitToken(_) => IntKind
        case IdentifierToken(_) => IdentifierKind
      }
    }
}

enum MyKind {
  case IntKind extends MyKind
  case IdentifierKind extends MyKind
}

object MyKind {
  import scala.quoted._
  given MyKindToExpr : ToExpr[MyKind] with {
    def apply(k:MyKind)(using Quotes): Expr[MyKind] = k match {
      case IntKind => '{IntKind}
      case IdentifierKind => '{IdentifierKind}
    }
  }
}