package ll1compiletime

import scala.quoted._
import scala.language.implicitConversions



enum MyToken {
    // Integer Token
    case IntT(i:Int) extends MyToken
    // separator
    case SepT extends MyToken
    // parenthese token
    case ParT(open:Boolean) extends MyToken 
}
enum MyKind {
    case IntK extends MyKind
    case SepK extends MyKind
    case ParK(open:Boolean) extends MyKind
}

given ToExpr[MyKind] with {
    def apply(k:MyKind)(using Quotes) = k match {
        case MyKind.IntK => '{MyKind.IntK}
        case MyKind.SepK => '{MyKind.SepK}
        case MyKind.ParK(o) => '{MyKind.ParK(${Expr(o)})}
    }
}

def getMyKind(t:MyToken):MyKind = t match {
    case MyToken.IntT(_) => MyKind.IntK
    case MyToken.SepT => MyKind.SepK
    case MyToken.ParT(o) => MyKind.ParK(o)
}

object P1 {
    import MyKind._
    import MyToken._

    private inline def parsingTable = ${init}

    private def init(using Quotes) = Expr(buildParsingTable(SyntaxDefTesting))

    object SyntaxDefTesting extends SyntaxDefinition[Int,MyToken,MyKind]{
        def getKind(t: Token) = getMyKind(t)

        inline def macroCall = parsingTable

        given Conversion[Char,CSyntax[Token]] with {
            def apply(c: Char) = c match {
                case '(' => elem(MyKind.ParK(true))
                case ')' => elem(MyKind.ParK(false))
                case ',' => elem(MyKind.SepK)
            }
        }

        val elemInt = accept(IntK){ case IntT(v) => v }
        val elemSep = elem(SepK)

        val rep: CSyntax[Seq[Int]] = repsep(elemInt | extractedSum ,elemSep)

        val sum: CSyntax[Int] = rep.map{ _.sum }

        lazy val extractedSum: CSyntax[Int] = recursive {
            ( '(' ~ sum ~ ')' ).map{
                case _ ~ s ~ _ => s
            }
        }

        lazy val entryPoint: CSyntax[Int] = extractedSum
    }
}

object P2 {
    import MyKind._
    import MyToken._

    private inline def parsingTable = ${init}

    private def init(using Quotes) = Expr(buildParsingTable(SyntaxDefTesting))

    object SyntaxDefTesting extends SyntaxDefinition[Int,MyToken,MyKind]{
        def getKind(t: Token) = getMyKind(t)

        inline def macroCall = parsingTable

        given Conversion[Char,CSyntax[Token]] with {
            def apply(c: Char) = c match {
                case '(' => elem(MyKind.ParK(true))
                case ')' => elem(MyKind.ParK(false))
                case ',' => elem(MyKind.SepK)
            }
        }

        val elemInt = accept(IntK){ case IntT(v) => v }
        val elemSep = elem(SepK)

        lazy val rep: CSyntax[Seq[Int]] = rep1sep(elemInt | extractedSum ,elemSep)

        lazy val sum: CSyntax[Int] = rep.map{ _.sum }

        lazy val extractedSum: CSyntax[Int] = recursive {
            ( '(' ~ opt(sum) ~ ')' ).map{
                case _ ~ Some(s) ~ _ => s
                case _ ~ None ~ _ => 0
            }
        }

        lazy val entryPoint: CSyntax[Int] = extractedSum
    }
}