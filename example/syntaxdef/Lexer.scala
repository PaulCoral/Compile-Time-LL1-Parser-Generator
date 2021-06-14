package example.syntaxdef

import scala.util.{Try,Success,Failure}

import silex._

import MyToken._

/**
 * A lexer for a sequence of number, each separated by a whitespace or a `,`
 * 
 * We use the [Silex](https://github.com/epfl-lara/silex) library, but a version 
 * [ported to Scala 3](https://github.com/PaulCoral/silex)
 */
object NumberLexer extends Lexers {

    type Token = MyToken

    type Position = StringPosition

    type Character = Char

    // definition of the lexer
    val lexer = Lexer(
        // `,` and white space are maped to `SeparatorToken`
        elem(',') |> {
            (_,_) => SeparatorToken
        },

        elem(_.isWhitespace) |> {
            (_,_) => SpaceToken
        },

        /** Digits are maped to IntLitToken when possible */
        many1(elem(_.isDigit)) |> {
            (cs, _) => { 
                Try(cs.mkString.toInt) match {
                    case Success(v) => IntLitToken(v)
                    case Failure(e) => ErrorToken(e.toString)
                }
            }
        }
    ) onError {
        // set errors
        (cs, _) => ErrorToken(cs.mkString)
    } onEnd {
        // put and end of file token
        (_) => EOFToken
    }

    /** 
     * Lex the given string, filtering out the separator
     * 
     * @param msg the string to lex
     */
    def apply(msg: String):List[Token] = {
        val source = Source.fromString(msg)
        lexer(source).toList.filter{ 
            case SpaceToken => false
            case _ => true
        }
    }

}