import syntax.Syntaxes
import syntax.TokensAndKinds._
import syntax.TokensAndKinds.Token._
import syntax.TokensAndKinds.Kind._

@main def hello: Unit = {
    println("Hello world!")
    println(Parser.rec_test)
    println(Parser.rec_test.first)
    println("finsihed")
}

object Parser extends Syntaxes:
    type Kind = syntax.TokensAndKinds.Kind
    type Token = syntax.TokensAndKinds.Token

    def getKind(t:Token): Kind = syntax.TokensAndKinds.getKind(t)

    lazy val elemInt: Syntax[Int] = accept(IntKind){ case IntLitToken(v) => v }

    lazy val elemId: Syntax[String] = accept(IdentifierKind){ case IdentifierToken(v) => v }

    lazy val var_assignation = elemId ~>~ elemInt

    lazy val var_or_litteral = elemInt | epsilon(0)

    lazy val rec_test: Syntax[Int] = recursive{ (elemInt ~>~ rec_test) | epsilon(0) }