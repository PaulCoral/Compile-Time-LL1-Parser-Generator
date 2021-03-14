package ast

type UpExpr = Expr | Int

trait Expr:
   def eval:Int

object Const:
   transparent inline def apply(inline i : Int) : Int = i

case class FinalConst(i: Int) extends Expr:
   def eval = i

case class Var(name: String, value: Expr) extends Expr:
   def eval = value.eval
object Var:
   def apply(name: String, value: Int) = new Var(name,FinalConst(value))

case class Plus(left : Expr, right: Expr):
   def eval = left.eval + right.eval
object Plus:
   def apply(l : Expr, r: Expr) = new Plus(l,r)
   def apply(l : Expr, r: Int) = new Plus(l,FinalConst(r))
   def apply(l : Int, r: Expr) = new Plus(FinalConst(l),r)
   transparent inline def apply(inline l: Int, inline r: Int) = l + r