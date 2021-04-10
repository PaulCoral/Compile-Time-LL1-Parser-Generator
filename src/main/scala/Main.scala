import syntax.Syntaxes
import syntax.TokensAndKinds._
import syntax.TokensAndKinds.Token._
import syntax.TokensAndKinds.Kind._
import compiletimeerror.CompileTimeError._
@main def hello: Unit =
    println("Hello world!")
    /* change to *true* to turn on compile time error (metals linting should raise an error) */
    compileTimeError(true, "Some test error message (change boolean to turn on/off)")
    println(Parser.var_assignation)
    println("finsihed")

object Parser extends Syntaxes:
    type Kind = syntax.TokensAndKinds.Kind
    type Token = syntax.TokensAndKinds.Token

    def getKind(t:Token): Kind = syntax.TokensAndKinds.getKind(t)

    lazy val elemInt: Syntax[Int] = accept(IntKind){ case IntLitToken(v) => v }

    lazy val elemId: Syntax[String] = accept(IdentifierKind){ case IdentifierToken(v) => v }

    lazy val var_assignation = elemId ~ elemInt

    lazy val var_or_litteral = elemInt | epsilon(0)

    lazy val rec_test = recursive{ elemInt | epsilon(0) }