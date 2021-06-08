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

inline def getPartialParsingTable = ${init}

def init(using Quotes) = {
    val parsing = new Parsing
    Expr(parsing(SyntaxDef.entryPoint))
}