package ll1compiletime.syntax

import ll1compiletime.parser.PartialParsingTable
import ll1compiletime.parser.ParsingTable

import scala.quoted.ToExpr

/**
 * The trait to define a syntax
 * 
 * @tparam A the return type of the entry point
 * @tparam T the Token type
 * @tparam K the Kind type
 */
trait SyntaxDefinition[A,T,K] {
    /** the type of parsing Tokens */
    type Token = T
    /** the type of Kind of the parsing Tokens */
    type Kind = K

    /** a given IdCounter that gives unique ids to Syntaxes */
    given idc:IdCounter

    /**
     * Type of a syntax in this context of [[SyntaxDefinition]]
     * 
     * @tparam X the return value of the syntax
     */
    type CSyntax[X] = Syntax[X,Token,Kind]

    /**
     * Return the Kind of a given Token
     * 
     * @param t the given Token
     * 
     * @return the Kind corresponding to the token
     */
    def getKind(t: Token):Kind

    lazy val entryPoint: CSyntax[A]

    /* --- Syntax Context : we restrain the context of the syntax for these Token `T` and kind `K` --- */


    // Extension to create `~` sequence of values
    extension [A](thiz:A) {
        /**
         * Return a sequence `~` with `thiz` first and then `that`
         */
        infix def ~[B](that:B): A ~ B = new ~(thiz,that)
    }


    extension [X](thiz: CSyntax[X]) {

        /**
         * Syntax Disjunction operator
         * 
         * Returns a new syntax which is the disjunction of syntaxes `thiz`
         * and `that`
         * 
         * @param thiz a sytnax of the disjunction
         * @param that the other syntax of the disjunction
         */
        infix def |(that: CSyntax[X])(using IdCounter):CSyntax[X] = 
            thiz.|(that)

        /**
         * Syntax Sequence operator
         * 
         * Returns a new syntax which is the sequence of syntaxes `thiz`
         * and then `that`
         * 
         * @param thiz first element of the sequence
         * @param that second element of the sequence
         */
        infix def ~[B](that: CSyntax[B]):CSyntax[X ~ B] = 
            thiz.~(that)

        /**
         * Sequence operator, keeping the left value
         * 
         * Returns a new syntax which is the sequence of syntaxes `thiz`
         * and then `that`, keeping only the value of `thiz`.
         * 
         * @param thiz first element of the sequence
         * @param that second element of the sequence
         */
        infix def ~<~[B](that: CSyntax[B]):CSyntax[X] = 
            thiz.~<~(that)
        /**
         * Sequence operator, keeping the right value
         * 
         * Returns a new syntax which is the sequence of syntaxes `thiz`
         * and then `that`, keeping only the value of `that`.
         * 
         * @param thiz first element of the sequence
         * @param that second element of the sequence
         */
        infix def ~>~[B](that: CSyntax[B]):CSyntax[B] = 
            thiz.~>~(that)

        /**
         * Map this syntax of `X` to a syntax of `B`
         * by applying the function `f` on resulting the value
         * 
         * @param f the function applied on the parsed value
         */
        def map[B](f: X => B): CSyntax[B] =
            thiz.map(f)

        /**
         * Syntax Sequence operator
         * 
         * Returns a new syntax which is the sequence of syntaxes `thiz`
         * and then `that`, prepend the parsed value of `thiz` to the
         *sequence of value of `that`
         * 
         * @param thiz first element of the sequence
         * @param that second element of the sequence, values are prepended to
         */
        def +:(that: CSyntax[Seq[X]]):CSyntax[Seq[X]] = 
            thiz.~(that).map{
                case x ~ xs => x +: xs
            }

    }

    extension [X](thiz: CSyntax[Seq[X]]) {

        /**
         * Syntax Sequence operator
         * 
         * Returns a new syntax which is the sequence of syntaxes `thiz`
         * and then `that`, append the parsed value of `that` to the
         *sequence of value of `thiz`
         * 
         * @param thiz first element of the sequence, values are appended to
         * @param that second element of the sequence
         */
        def :+(that: CSyntax[X]):CSyntax[Seq[X]] = 
            thiz.~(that).map{
                case xs ~ x => xs :+ x
            }
    }
    

    /**
     * A Syntax that accept a Token of the given Kind and applies
     * applies the function `f` on the parsed Token
     * 
     * @tparam B the resulting type when parsing the given kind
     * @param k the kind of the Token to parse
     * @param f the partial function to apply to a parsed Token of the kind
     */
    def accept[B](k:Kind)(f: PartialFunction[Token,B])(using IdCounter): CSyntax[B] =
        Syntax.accept(k)(f)

    /**
     * A nullable sytnax which gives the given value on an empty
     * sequence of Token
     * 
     * @tparam B the type of the value to produce
     * @param e the value to produce
     */
    def epsilon[B](e: B)(using IdCounter): CSyntax[B] = 
        Syntax.epsilon[B,Token,Kind](e)

    /**
     * The empty syntax, results in parsing failure
     * 
     * @tparam B the type of value that should be produced
     */
    def failure[B](using IdCounter): CSyntax[B] = Syntax.failure[B,Token,Kind]


    /**
     * A syntax that produce the parsed `Token` of the given `Kind`
     * 
     * @param k the Kind of the Token to parse
     */
    def elem(k: Kind)(using IdCounter): CSyntax[Token] = 
        Syntax.elem[Token,Kind](k:Kind)

    /**
     * Define a recursive Syntax
     * 
     * @tparam B the type of the parsed value
     * @param syntax the inner recursive syntax
     */
    def recursive[B](syntax: => CSyntax[B])(using IdCounter): CSyntax[B] = 
        Syntax.recursive[B,Token,Kind](syntax)

    
    /**
     * Define an optional syntax.
     * 
     * If the parsing succeed the value is of type `Some[B]`
     * Otherwise it is of type `None`
     */
    def opt[B](cs: CSyntax[B]):CSyntax[Option[B]] = cs.opt


    /**
     * The syntax the represent 0 or more repetition of the given
     * syntax
     * 
     * @tparam A the type of the syntax to repeat
     * @param rep the syntax to be repeated
     */
    def many[A](rep: CSyntax[A])(using IdCounter): CSyntax[Seq[A]] = {
        lazy val rest: CSyntax[Seq[A]] = recursive {
            ((rep +: rest) | epsilon(List()))
        }
        rest
    }


    /**
     * The syntax the represent 1 or more repetition of the given
     * syntax
     * 
     * @tparam A the type of the syntax to repeat
     * @param rep the syntax to be repeated
     */
    def many1[A](rep: CSyntax[A])(using IdCounter): CSyntax[Seq[A]] = {
        rep +: many(rep)
    }

    /**
     * The syntax the represent 0 or more repetition of the given
     * syntax `rep`, each separated by a syntax `sep`. The resulting
     * syntax is a sequence of `rep`s
     * 
     * @tparam A the type of the syntax to repeat
     * @tparam B the type of the separator syntax
     * @param rep the syntax to be repeated
     * @param sep the separator syntax
     */
    def repsep[A, B](rep: CSyntax[A], sep: CSyntax[B])(using IdCounter): CSyntax[Seq[A]] ={
        (rep1sep(rep, sep) | epsilon(Vector()))
    }

    /**
     * The syntax the represent 1 or more repetition of the given
     * syntax `rep`, each separated by a syntax `sep`. The resulting
     * syntax is a sequence of `rep`s
     * 
     * @tparam A the type of the syntax to repeat
     * @tparam B the type of the separator syntax
     * @param rep the syntax to be repeated
     * @param sep the separator syntax
     */
    def rep1sep[A, B](rep: CSyntax[A], sep: CSyntax[B])(using IdCounter): CSyntax[Seq[A]] = {
        lazy val rest: CSyntax[Seq[A]] = recursive {
            (sep ~>~ (rep +: rest)) | epsilon(Vector())
        }
        rep +: rest
    }

    /**
     * Return a syntax that is a disjunction of the given syntaxes
     * 
     * @example 
     * oneOf[X](a,b,...,n)
     * // can be seen as
     * (a | b | ... | n | failure[X])
     * 
     * @tparam A the type of the disjunction syntaxes
     * @param syntaxes the syntax to create the disjunction from
     * 
     */
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
