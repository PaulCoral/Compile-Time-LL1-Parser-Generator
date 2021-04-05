package simpleparser

import com.google.protobuf.Descriptors.FileDescriptor.Syntax

/*
  AST minimal: Epsilon, Failure, Elem, Sequence, Disjunction, (Transform ?), et Recursive.
  lazy val manyAs: Syntax[Unit] = recursive { epsilon(()) | elem(kindA) ~>~ manyAs }
 */

sealed trait Token

case class IntLitToken(value: Int) extends Token

case class OperatorToken(value: String) extends Token



sealed trait Syntax[A]

case class Epsilon[A]() extends Syntax[A]

case class Failure[A]() extends Syntax[A]

case class Elem[A](e: A) extends Syntax[A]

case class Sequence[A,B](left: Syntax[A], right: Syntax[B]) extends Syntax[(A,B)]

case class Disjunction[A](left: Syntax[A], right: Syntax[A]) extends Syntax[A]




class SimpleParser 
