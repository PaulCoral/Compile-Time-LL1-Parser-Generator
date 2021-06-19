package json

import ll1compiletime._
import json._
import json.TokenClass.given

import scala.quoted._
import scala.language.implicitConversions
import javax.swing.JPopupMenu.Separator
import ll1compiletime.parser.PartialParsingTable


private inline def parsingTable = ${init}

private def init(using Quotes) = Expr(buildParsingTable(SyntaxDef))

object SyntaxDef extends ll1compiletime.syntax.CompileTime[json.Value,json.Token,json.TokenClass]{
    override def getKind(token: Token): TokenClass = token match {
        case SeparatorToken(value, _) => SeparatorClass(value)
        case BooleanToken(_, _) => BooleanClass
        case NumberToken(_, _) => NumberClass
        case StringToken(_, _) => StringClass
        case NullToken(_) => NullClass
        case _ => NoClass
    }

    inline def macroCall:PartialParsingTable[Kind] = parsingTable

    val booleanValue: CSyntax[Value] = accept(BooleanClass) {
        case BooleanToken(value, range) => BooleanValue(value, range)
    }

    val numberValue: CSyntax[Value] = accept(NumberClass) {
        case NumberToken(value, range) => NumberValue(value, range)
    }

    val stringValue: CSyntax[StringValue] = accept(StringClass) {
        case StringToken(value, range) => StringValue(value, range)
    }

    val nullValue: CSyntax[Value] = accept(NullClass) {
        case NullToken(range) => NullValue(range)
    }

    given Conversion[Char, CSyntax[Token]] with {
        def apply(c: Char):CSyntax[Token] = elem(SeparatorClass(c))
    }

    lazy val arrayValue: CSyntax[Value] =
        ('[' ~ repsep(value, ',') ~ ']').map {
        case start ~ vs ~ end => ArrayValue(vs, (start.range._1, end.range._2))
        }

    lazy val binding: CSyntax[(StringValue, Value)] =
        (stringValue ~ ':' ~ value).map {
        case key ~ _ ~ value => (key, value)
        }

    lazy val objectValue: CSyntax[Value] =
        ('{' ~ repsep(binding, ',') ~ '}').map {
            case start ~ bs ~ end => ObjectValue(bs, (start.range._1, end.range._2))
        }

    lazy val value: CSyntax[Value] = recursive {
        oneOf(
        arrayValue,
        objectValue,
        booleanValue,
        numberValue,
        stringValue.up[Value],
        nullValue)
    }

    lazy val entryPoint = value
}