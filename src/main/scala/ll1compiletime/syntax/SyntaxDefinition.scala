package ll1compiletime.syntax

import ll1compiletime.parser.PartialParsingTable
import ll1compiletime.parser.ParsingTable

import scala.quoted.ToExpr

trait SyntaxDefinition[A,T,K] {
    type Token = T
    type Kind = K

    type CSyntax[X] = Syntax[X,Token,Kind]

    def getKind(t: Token):Kind

    lazy val entryPoint: CSyntax[A]

    inline def compileTimeParsingTable: PartialParsingTable[A,Kind]

    final inline def parse:ParsingTable[A,Token,Kind] = compileTimeParsingTable.withFunctionTable(this)

    /* 
     * Syntax Context : we restrain the context of the syntax for tokens and kinds   
     */

    
    extension [A](thiz:A) {
        infix def ~[B](that:B):A ~ B = new ~(thiz,that)
    }


    extension [X](thiz: CSyntax[X]) {
        infix def |(that: CSyntax[X])(using IdCounter):CSyntax[X] = 
            thiz.|(that)

        /**
         * Sequence operator
         */
        infix def ~[B](that: CSyntax[B]):CSyntax[X ~ B] = 
            thiz.~(that)

        /**
         * Sequence operator, keeping the left value
         */
        infix def ~<~[B](that: CSyntax[B]):CSyntax[X] = 
            thiz.~<~(that)
        /**
         * Sequence operator, keeping the right value
         */
        infix def ~>~[B](that: CSyntax[B]):CSyntax[B] = 
            thiz.~>~(that)

        /**
         * Map this syntax to another
         */
        def map[B](f: X => B): CSyntax[B] =
            thiz.map(f)

    }

    extension [X](thiz: CSyntax[Seq[X]]) {
        def :+(that: CSyntax[X]):CSyntax[Seq[X]] = 
            thiz.~(that).map{
                case a ~ b => a :+ b
            }

        def +:(that: CSyntax[X]):CSyntax[Seq[X]] = 
            that.~(thiz).map{
                case b ~ a => b +: a
            }
    }
    


    def accept[B](k:Kind)(f: PartialFunction[Token,B])(using IdCounter): CSyntax[B] =
        Syntax.accept(k)(f)

    def epsilon[B](e: B)(using IdCounter): CSyntax[B] = 
        Syntax.epsilon[B,Token,Kind](e)

    def failure[B](using IdCounter): CSyntax[B] = Syntax.failure[B,Token,Kind]

    def elem(k: Kind)(using IdCounter): CSyntax[Token] = 
        Syntax.elem[Token,Kind](k:Kind)

    def recursive[B](syntax: => CSyntax[B])(using IdCounter): CSyntax[B] = 
        Syntax.recursive[B,Token,Kind](syntax)

    def opt[B](cs: CSyntax[B]):CSyntax[Option[B]] = cs.opt

    def many[A](rep: CSyntax[A])(using IdCounter): CSyntax[Seq[A]] = {
        lazy val rest: CSyntax[Seq[A]] = recursive {
            ((rep.+:(rest)) | epsilon(List()))
        }
        rest
    }

    def many1[A](rep: CSyntax[A])(using IdCounter): CSyntax[Seq[A]] = {
        rep.+:(many(rep))
    }

    def repsep[A, B](rep: CSyntax[A], sep: CSyntax[B])(using IdCounter): CSyntax[Seq[A]] ={
        (rep1sep(rep, sep) | epsilon(Vector()))
    }

    def rep1sep[A, B](rep: CSyntax[A], sep: CSyntax[B])(using IdCounter): CSyntax[Seq[A]] = {
        lazy val rest: CSyntax[Seq[A]] = recursive {
            (sep ~>~ (rep.+:(rest))) | epsilon(Vector())
        }
        rep.+:(rest)
    }

    def oneOf[A](syntaxes: CSyntax[A]*)(using IdCounter): CSyntax[A] = {
        var queue = syntaxes.toVector :+ failure[A]

        while (queue.size > 1) {
            val a = queue(0)
            val b = queue(1)
            queue = queue.drop(2)
            queue :+= a | b
        }

        queue.head
    }

}
