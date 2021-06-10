package example.syntaxdef

import scala.quoted._

import ll1compiletime.syntax.Syntax
import ll1compiletime.syntax.Syntax._
import ll1compiletime.parser._

import SyntaxDef.given

inline def getPartialParsingTable = ${init}

def init(using Quotes) = {
    val parsing = new Parsing[SyntaxDef.Kind]
    Expr(parsing(SyntaxDef.entryPoint))
}