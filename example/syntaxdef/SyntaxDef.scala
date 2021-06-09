package example.syntaxdef

import syntax.IdCounter
import syntax.Syntax
import syntax.Syntax._
import syntax.getPos
import syntax.SyntaxDefinition

import parser._

//import example.syntaxdef.getPartialParsingTable

import scala.quoted._


object SyntaxDef extends SyntaxDefinition[Int,MyToken,MyKind] {
    import MyToken._
    import MyKind._

    def getKind(t:MyToken):MyKind = MyToken.getKind(t)

    given id:IdCounter = new IdCounter()

    lazy val elemInt: Syntax[Int,Token,Kind] = accept(IntKind){ case IntLitToken(v) => v }

    lazy val elemId: Syntax[String,Token,Kind] = accept(IdentifierKind){ case IdentifierToken(v) => v }

    lazy val var_assignation:Syntax[String,Token,Kind] = (elemId ~ elemInt).map{ case (i,v) => s"$i = $v" }

    lazy val many_var_assignation:Syntax[String,Token,Kind] = (var_assignation ~ (var_assignation)).map{ case (v1,v2) => s"$v1\n$v2" }

    lazy val rec_sum: Syntax[Int,Token,Kind] = recursive{ sum | epsilon(0) }

    lazy val sum: Syntax[Int,Token,Kind] = (elemInt ~ rec_sum).map{ case (a,b) => a + b }

    lazy val innerManyAs = epsilon(()) | elem(IntKind) ~>~ manyAs

    lazy val manyAs: Syntax[Unit,Token,Kind] = recursive { innerManyAs }

    lazy val nullableConflict = (epsilon(0) | epsilon(1))|(epsilon(0)|epsilon(1))

    lazy val firstFirst = elemId | elemId

    lazy val firstFirstRec = recursive{ elemId }

    lazy val snfConflict = (epsilon(1) | elemInt) ~ elemInt

    lazy val complexFirstFirst = (((elemId ~>~ elemInt) | (elemInt ~>~ elemInt)) | ((elemId ~>~ elemInt) | (elemInt ~>~ elemInt)))


    lazy val entryPoint = sum

    inline def parse = getPartialParsingTable.withFunctionTable(entryPoint,getKind)

    def parseRuntime = {
        val parsing = new Parsing[Kind]
        parsing(entryPoint)
        getPartialParsingTable.withFunctionTable(entryPoint,getKind)
    }


    given anyToExpr:ToExpr[Any] with {
        def apply(a: Any)(using Quotes) = a match {
            case x:Int => Expr(x)
            case x:String => Expr(x)
            case _ => throw MatchError(a)
        }
    }
}



