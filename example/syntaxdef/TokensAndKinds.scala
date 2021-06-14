package example.syntaxdef


/** The Tokens */
enum MyToken {
  /** 
   * Integer Literal token wih its value 
   * 
   * @param value its value
   */
  case IntLitToken(value: Int) extends MyToken
  /** Space Token */
  case SpaceToken extends MyToken
  /** Separator Token */
  case SeparatorToken extends MyToken
  /** 
   * Error Token with an error msg 
   * 
   * @param msg the error message
   */
  case ErrorToken(msg: String) extends MyToken
  /** End of file Token */
  case EOFToken extends MyToken
}
/** The Kinds */
enum MyKind {
  /** Integer Kind */
  case IntKind extends MyKind
  /** Space Kind */
  case SpaceKind extends MyKind
  /** Separator Kind */
  case SeparatorKind extends MyKind
  /** Error Kind */
  case ErrorKind extends MyKind
  /** End of file Kind */
  case EOFKind extends MyKind
}

object MyKind {
  import scala.quoted._

  /** Get an `quoted.Expr` from MyKind */
  given MyKindToExpr : ToExpr[MyKind] with {
    def apply(k:MyKind)(using Quotes): Expr[MyKind] = k match {
      case IntKind => '{IntKind}
      case SpaceKind => '{SpaceKind}
      case SeparatorKind => '{SeparatorKind}
      case ErrorKind => '{ErrorKind}
      case EOFKind => '{EOFKind}
    }
  }

  /**
   * Give the kind corresponding to the token
   * 
   * @param t the Token
   * @return the corresponding Kind
   */
  def getKind(t:MyToken): MyKind = {
    import MyToken._
    import MyKind._
    t match {
      case IntLitToken(_) => IntKind
      case SpaceToken => SpaceKind
      case SeparatorToken => SeparatorKind
      case ErrorToken(_) => ErrorKind
      case EOFToken => EOFKind
    }
  }
}