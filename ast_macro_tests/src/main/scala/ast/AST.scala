package ast

sealed trait Expr[A]:
    lazy val eval:A

type Num = Int

case class Const(x: Num) extends Expr[Num]:
    lazy val eval: Num = x
    override def toString = x.toString

abstract class BinOp(left:Expr[Num], right:Expr[Num], f:(Num,Num) => Num) extends Expr[Num] :
    lazy val eval: Num = f(left.eval,right.eval)
    
case class Plus(left:Expr[Num], right:Expr[Num]) extends BinOp(left, right,_+_):
    override def toString = s"($left + $right)"
case class Minus(left:Expr[Num], right:Expr[Num]) extends BinOp(left, right,_-_):
    override def toString = s"($left - $right)"
case class Times(left:Expr[Num], right:Expr[Num]) extends BinOp(left, right,_*_):
    override def toString = s"($left * $right)"
case class Div(left:Expr[Num], right:Expr[Num]) extends BinOp(left, right,_/_):
    override def toString = s"($left / $right)"
