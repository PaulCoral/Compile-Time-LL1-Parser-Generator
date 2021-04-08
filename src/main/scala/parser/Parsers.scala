package parser

import syntax.Syntaxes

trait Parsers:
    self: Syntaxes => 

    trait Parser[A]:
        lazy val syntax: Syntax[A]
