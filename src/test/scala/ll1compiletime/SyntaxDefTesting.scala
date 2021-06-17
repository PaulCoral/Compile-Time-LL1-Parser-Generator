package ll1compiletime

import scala.quoted._
import scala.language.implicitConversions

import MyKind._
import MyToken._


object P1 {
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

        lazy val rep: CSyntax[Seq[Int]] = repsep(elemInt | extractedSum ,elemSep)

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
    private inline def parsingTable = ${init}

    private def init(using Quotes) = Expr(buildParsingTable(SyntaxDefTesting))

    object SyntaxDefTesting extends SyntaxDefinition[Seq[Int],MyToken,MyKind]{
        def getKind(t: Token) = getMyKind(t)

        inline def macroCall = parsingTable

        val elemInt = accept(IntK){ case IntT(v) => v }

        lazy val entryPoint = elemInt +: epsilon(List(0))
    }
}