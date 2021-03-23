package ast

import scala.quoted._
import scala.quoted.Quotes

trait Exp

case class Const(v: Int) extends Exp
case class Plus(left: Exp, right: Exp) extends Exp

transparent inline def eval(inline x: Exp) = ${ evaluate('{x}) }
def evaluate(expr: Expr[Exp])(using Quotes):Expr[Int] = 
    import quotes.reflect.*
    expr match
        case '{Const($x)} => x
        case '{Plus($x,$y)} => '{${evaluate(x)} + ${evaluate(y)}}