package parser

import scala.quoted._
import syntax.Syntax
import syntax.Syntax._
import syntax.TokensAndKinds.Kind
import syntax.TokensAndKinds.Kind._
import syntax.TokensAndKinds.Token
import syntax.TokensAndKinds.Token._

inline def parsingTable() = ${init}

def init(using Quotes) = {
    object SyntaxDef {
        def getKind(t:Token): Kind = syntax.TokensAndKinds.getKind(t)

        lazy val elemInt: Syntax[Int] = accept(IntKind){ case IntLitToken(v) => v }

        lazy val elemId: Syntax[String] = accept(IdentifierKind){ case IdentifierToken(v) => v }

        lazy val var_assignation = (elemId ~ elemInt).map{ case (i,v) => s"$i = $v" }

        lazy val many_var_assignation = (var_assignation ~ (var_assignation)).map{ case (v1,v2) => s"$v1\n$v2" }

        lazy val var_or_litteral = elemInt | epsilon(0)

        lazy val rec_sum: Syntax[Int] = recursive{ rec_sum_seq | epsilon(0) }

        lazy val rec_sum_seq: Syntax[Int] = (elemInt ~ rec_sum).map{ case (a,b) => a + b }

        lazy val manyAs: Syntax[Unit] = recursive { epsilon(()) | elem(IntKind) ~>~ manyAs }

        lazy val nullableConflict = epsilon(0) | epsilon(1)

        lazy val firstFirst = elemId | elemId

        lazy val firstFirstRec = recursive{ elemId | elemId }

        lazy val snfConflict = (epsilon(1) | elemInt) ~ elemInt
    }

    val res = Parsing(SyntaxDef.firstFirst)
    scala.quoted.Expr(s"$res")
}


