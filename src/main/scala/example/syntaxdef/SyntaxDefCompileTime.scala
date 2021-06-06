package example.syntaxdef

import scala.quoted._

import syntax.Syntax
import syntax.Syntax._
import syntax.TokensAndKinds.Token._
import syntax.TokensAndKinds.Kind._
import syntax.TokensAndKinds.Token
import syntax.TokensAndKinds.Token._
import parser.Parsing

inline def parsingTable = ${init}

def init[A](using Quotes) = {

    val tokens = List(IntLitToken(1),IntLitToken(2))
    val syntax = SyntaxDef.sum
    
    val parser = Parsing(syntax)
    Expr(parser(tokens).toString)
}


