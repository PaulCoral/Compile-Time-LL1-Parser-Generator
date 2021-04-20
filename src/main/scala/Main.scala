/* import syntax.Syntaxes
import parser.Parsing
import syntax.TokensAndKinds._
import syntax.TokensAndKinds.Token._
import syntax.TokensAndKinds.Kind._
import parser.Parsing */

@main def hello: Unit = println("hello world")

/* @main def hello: Unit = {
    println("Hello world!")
    val a: Parser.ParsingResult[String] = Parser(List(IdentifierToken("var1"),IntLitToken(2),IdentifierToken("var2"),IntLitToken(3)))
    println("Result : ")
    println(
        a match
            case Parser.ParsingResult.ParsingSuccess(v) => v
            case _ => a
    )
    println("finsihed")
} */

/* object Parser extends Syntaxes with Parsing[String]:
    type Kind = syntax.TokensAndKinds.Kind
    type Token = syntax.TokensAndKinds.Token

    def entryPoint: Syntax[String] = many_var_assignation

    def getKind(t:Token): Kind = syntax.TokensAndKinds.getKind(t)

    lazy val elemInt: Syntax[Int] = accept(IntKind){ case IntLitToken(v) => v }

    lazy val elemId: Syntax[String] = accept(IdentifierKind){ case IdentifierToken(v) => v }

    lazy val var_assignation = (elemId ~ elemInt).map{ case (i,v) => s"$i = $v" }

    lazy val many_var_assignation = (var_assignation ~ (var_assignation)).map{ case (v1,v2) => s"$v1\n$v2" }

    lazy val var_or_litteral = elemInt | epsilon(0)

    lazy val rec_sum: Syntax[Int] = recursive{ rec_sum_seq | epsilon(0) }

    lazy val rec_sum_seq: Syntax[Int] = (elemInt ~ rec_sum).map{ case (a,b) => a + b }

    lazy val manyAs: Syntax[Unit] = recursive { epsilon(()) | elem(IntKind) ~>~ manyAs } */