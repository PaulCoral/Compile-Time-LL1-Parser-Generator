package parser

import syntax.Syntaxes

trait Parsers:
    self: Syntaxes =>
    case class Parser[A](entryPoint: Syntax[A]):

        type Stack[A] = List[A]

        def apply[A](tokens: List[Token]): A  = ???

        def next[A](tokens: List[Token], syntaxStack: Stack[Syntax[?]], resultStack: Stack[ParsingResult[?]]) = 
            val (x::xs) = syntaxStack
            val (t::ts) = tokens
            x match
                case Success(v) => (xs,Success(v)::resultStack)
                case Failure() => throw Error(s"Parsing failed with token $t")
                case Elem(k) => ???
                case Transform(s, f) => ???
                case Sequence(l,r) => 
                    (
                        l :: r :: xs,
                        resultStack
                    )
                case Disjunction(l,r) => 
                    if l.first.contains(getKind(t)) then
                        (l :: xs,resultStack)
                    else
                        (r :: syntaxStack, resultStack)
                case Recursive(_,_) => ???
                case _ => ???

        def reduce(result: Stack[ParsingResult[?]]): Stack[ParsingResult[?]] = ???

    object Parser:
        def apply[A](syntax: Syntax[A]) = 
            if syntax.hasConflict then
                throw Error()
            else
                new Parser(syntax)

    enum ParsingResult[A]:
        case Success[A](value: A) extends ParsingResult[A]
        case EmptySequence[A,B]() extends ParsingResult[(A,B)]
        case PartialSequence[A,B](value: A) extends ParsingResult[(A,B)]
        case PartialTransform[A,B](f: A => B) extends ParsingResult[B]