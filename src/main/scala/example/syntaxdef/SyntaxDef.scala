package example.syntaxdef

import syntax.IdCounter
import syntax.Syntax
import syntax.Syntax._
import syntax.TokensAndKinds.Kind._
import syntax.TokensAndKinds.Token._
import syntax.getPos

import scala.quoted._

import example.syntaxdef.parsingTable

object SyntaxDef {
    private given id:IdCounter = new IdCounter()

    val elemInt: Syntax[Int] = accept(IntKind){ case IntLitToken(v) => v }

    val elemId: Syntax[String] = accept(IdentifierKind){ case IdentifierToken(v) => v }

    val var_assignation:Syntax[String] = (elemId ~ elemInt).map{ case (i,v) => s"$i = $v" }

    val many_var_assignation:Syntax[String] = (var_assignation ~ (var_assignation)).map{ case (v1,v2) => s"$v1\n$v2" }

    val rec_sum: Syntax[Int] = recursive{ sum | epsilon(0) }

    val sum: Syntax[Int] = (elemInt ~ rec_sum).map{ case (a,b) => a + b }

    val innerManyAs = epsilon(()) | elem(IntKind) ~>~ manyAs

    val manyAs: Syntax[Unit] = recursive { innerManyAs }

    val nullableConflict = (epsilon(0) | epsilon(1))|(epsilon(0)|epsilon(1))

    val firstFirst = elemId | elemId

    val firstFirstRec = recursive{ elemId }

    val snfConflict = (epsilon(1) | elemInt) ~ elemInt

    val complexFirstFirst = (((elemId ~>~ elemInt) | (elemInt ~>~ elemInt)) | ((elemId ~>~ elemInt) | (elemInt ~>~ elemInt)))

    def parse = parsingTable

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