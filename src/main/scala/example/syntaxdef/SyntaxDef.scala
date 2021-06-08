package example.syntaxdef

import syntax.IdCounter
import syntax.Syntax
import syntax.Syntax._
import syntax.TokensAndKinds.Kind._
import syntax.TokensAndKinds.Token._
import syntax.getPos

import parser.ParsingTable

import scala.quoted._

import example.syntaxdef.parsingTable
import syntax.SyntaxDefinition

object SyntaxDef extends SyntaxDefinition[Unit]{
    given id:IdCounter = new IdCounter()

    lazy val elemInt: Syntax[Int] = accept(IntKind){ case IntLitToken(v) => v }

    lazy val elemId: Syntax[String] = accept(IdentifierKind){ case IdentifierToken(v) => v }

    lazy val var_assignation:Syntax[String] = (elemId ~ elemInt).map{ case (i,v) => s"$i = $v" }

    lazy val many_var_assignation:Syntax[String] = (var_assignation ~ (var_assignation)).map{ case (v1,v2) => s"$v1\n$v2" }

    lazy val rec_sum: Syntax[Int] = recursive{ sum | epsilon(0) }

    lazy val sum: Syntax[Int] = (elemInt ~ rec_sum).map{ case (a,b) => a + b }

    lazy val innerManyAs = epsilon(()) | elem(IntKind) ~>~ manyAs

    lazy val manyAs: Syntax[Unit] = recursive { innerManyAs }

    lazy val nullableConflict = (epsilon(0) | epsilon(1))|(epsilon(0)|epsilon(1))

    lazy val firstFirst = elemId | elemId

    lazy val firstFirstRec = recursive{ elemId }

    lazy val snfConflict = (epsilon(1) | elemInt) ~ elemInt

    lazy val complexFirstFirst = (((elemId ~>~ elemInt) | (elemInt ~>~ elemInt)) | ((elemId ~>~ elemInt) | (elemInt ~>~ elemInt)))


    lazy val entryPoint = manyAs

    def partialParsingTable = parsingTable

    def parse = partialParsingTable.withFunctionTable(this)


    given ToExpr[Any] with {
        def apply(a: Any)(using Quotes) = a match {
            case x:Int => Expr(x)
            case x:String => Expr(x)
            case _:Unit => '{()}
            case _ => throw MatchError(a)
        }
    }

    given ToExpr[Unit] with {
        def apply(u: Unit)(using Quotes) = '{()}
    }
}