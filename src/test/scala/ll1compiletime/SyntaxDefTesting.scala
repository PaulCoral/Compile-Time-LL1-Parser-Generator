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


trait SyntaxUtils extends SyntaxDefinition[MyToken,MyKind]{
    import MyKind._
    import MyToken._
    
    given conv(using IdCounter):Conversion[Char,CSyntax[Token]] with {
        def apply(c: Char) = c match {
            case '(' => elem(MyKind.ParK(true))
            case ')' => elem(MyKind.ParK(false))
            case ',' => elem(MyKind.SepK)
        }
    }

    val elemInt = accept(IntK){ case IntT(v) => v }
    val elemSep = elem(SepK)

}

object P1 {
    import MyKind._
    import MyToken._

    private inline def parsingTable = ${init}

    private def init(using Quotes) = Expr(buildParsingTable(SyntaxDefTesting))

    object SyntaxDefTesting extends CompileTime[Int,MyToken,MyKind] with SyntaxUtils{
        def getKind(t: Token) = getMyKind(t)

        inline def macroCall = parsingTable

        lazy val rep: CSyntax[Seq[Int]] = repsep(elemInt | extractedSum ,',')
        lazy val sum: CSyntax[Int] = rep.map{ _.sum }
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

    object SyntaxDefTesting extends CompileTime[Int,MyToken,MyKind] with SyntaxUtils {
        def getKind(t: Token) = getMyKind(t)

        inline def macroCall = parsingTable

        lazy val rep1: CSyntax[Seq[Int]] = rep1sep(elemInt | extractedSumOpt ,',')
        lazy val sum1: CSyntax[Int] = rep1.map{ _.sum }
        lazy val extractedSumOpt: CSyntax[Int] = recursive {
            ( '(' ~ opt(sum1) ~ ')' ).map{
                case _ ~ Some(s) ~ _ => s
                case _ ~ None ~ _ => 0
            }
        }

        lazy val entryPoint: CSyntax[Int] = extractedSumOpt
    }
}