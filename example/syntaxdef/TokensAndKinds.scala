package example.syntaxdef


enum MyToken {
  case IntLitToken(value: Int) extends MyToken
  case SeparatorToken extends MyToken
  case ErrorToken(msg: String) extends MyToken
  case EOFToken extends MyToken
}
enum MyKind {
  case IntKind extends MyKind
  case SeparatorKind extends MyKind
  case ErrorKind extends MyKind
  case EOFKind extends MyKind
}

object MyKind {
  import scala.quoted._
  given MyKindToExpr : ToExpr[MyKind] with {
    def apply(k:MyKind)(using Quotes): Expr[MyKind] = k match {
      case IntKind => '{IntKind}
      case SeparatorKind => '{SeparatorKind}
      case ErrorKind => '{ErrorKind}
      case EOFKind => '{EOFKind}
    }
  }

  def getKind(t:MyToken): MyKind = {
    import MyToken._
    import MyKind._
    t match {
      case IntLitToken(_) => IntKind
      case SeparatorToken => SeparatorKind
      case ErrorToken(_) => ErrorKind
      case EOFToken => EOFKind
    }
  }
}