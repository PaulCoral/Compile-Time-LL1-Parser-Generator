import syntax.Syntaxes
import syntax.TokensAndKinds._

@main def hello: Unit = {
    println("Hello world!")
    println("finsihed")
}

object Parser extends Syntaxes:
    type Kind = syntax.TokensAndKinds.Kind
    type Token = syntax.TokensAndKinds.Token

    def getKind(t:Token): Kind = syntax.TokensAndKinds.getKind(t)

    lazy val elemInt: Syntax[Int] = accept(IntKind){ case IntLitToken(v) => v }

    lazy val from_1_to_2 = elemInt ~ elemInt

    lazy val choose_1_or_2 = elemInt | elemInt

    lazy val choose_1_or_2_or_none = elemInt | epsilon(0)