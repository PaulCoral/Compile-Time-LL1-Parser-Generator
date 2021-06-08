package example.syntaxdef

import scala.quoted._

import syntax.Syntax
import syntax.Syntax._
import syntax.TokensAndKinds.Token._
import syntax.TokensAndKinds.Kind._
import syntax.TokensAndKinds.Token
import syntax.TokensAndKinds.Token._
import parser._

import SyntaxDef.given

inline def parsingTable = ${init}

def init[A](using Quotes) = {
    val syntax = SyntaxDef.sum
    
    val parsing = new Parsing

    val pt = parsing(syntax)
    //Expr(pt(tokens).toString)
    Expr(pt)
}


