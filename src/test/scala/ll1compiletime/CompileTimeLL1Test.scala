package ll1compiletime

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec

import scala.quoted._

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



class CompileTimeLL1Test extends AnyFlatSpec {
    import MyToken._
    import ParsingResult._

    val tokens = List(
        ParT(true),
            IntT(1),
            SepT,
            ParT(true),
                IntT(2),
            ParT(false),
        ParT(false)
    )

    "Sum parser" should "return 3 with rest" in {
        val parser = P1.SyntaxDefTesting.parser
        val result = parser(tokens.iterator)
        assert(
            result === ParsedSuccessfully(3)
        )
    }
}