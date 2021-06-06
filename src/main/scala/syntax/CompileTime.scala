package syntax

import scala.quoted._


inline def getPos(any: Any) = ${init('{any})}

def init(expr: Expr[Any])(using Quotes) = {
    import quotes.reflect._
    report.error("hello",expr.asTerm.pos)
    //error("test error",expr.asTerm.pos)
    Expr("test")
}