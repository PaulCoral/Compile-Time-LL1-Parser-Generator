package example.syntaxdef

import scala.util.{Try,Success,Failure}

import silex._

import MyToken._

object NumberLexer extends Lexers {
    type Token = MyToken

    type Position = StringPosition

    type Character = Char

    val lexer = Lexer(
        elem(',') | elem(_.isWhitespace) |> {
            (_,_) => SeparatorToken
        },

        many1(elem(_.isDigit)) |> {
            (cs, _) => { 
                Try(cs.mkString.toInt) match {
                    case Success(v) => IntLitToken(v)
                    case Failure(e) => ErrorToken(e.toString)
                }
            }
        }
    ) onError {
        (cs, _) => ErrorToken(cs.mkString)
    } onEnd {
        (_) => EOFToken
    }

    def apply(msg: String):List[Token] = {
        val source = Source.fromString(msg)
        lexer(source).toList.filter{ 
            case SeparatorToken => false
            case _ => true
        }
    }

}