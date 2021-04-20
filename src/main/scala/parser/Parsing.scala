package parser

import syntax.Syntaxes

trait Parsing[A]:
    self: Syntaxes =>
    
    type Stack[A] = List[A]

    def entryPoint: Syntax[A]

    def apply[A](tokens: List[Token]): ParsingResult[A]  = 
        import ResultStackElement._
        import ParsingResult._

        if entryPoint.hasConflict then
            HasConflict("has conflict")
        else
            val (t,s,r) = next(tokens,List(entryPoint), List())
            if r.length > 1 then
                throw Error(s"length result : $r")
            else
                r.head match
                    case Value(v) => ParsingResult.ParsingSuccess(v.asInstanceOf[A])
                    case _ => throw Error("")

    def next[A](
        tokens: List[Token],
        syntaxStack: Stack[Syntax[?]],
        resultStack: Stack[ResultStackElement]
    ): (List[Token], Stack[Syntax[?]], Stack[ResultStackElement]) = 
        import ResultStackElement._
        if syntaxStack.isEmpty || (tokens.isEmpty && !syntaxStack.head.isNullable) then
            (tokens,syntaxStack, reduce(resultStack))
        else
            val (newTokens, newSyntaxStack, newResultStack) = 
                val (x::xs) = syntaxStack
                if tokens.isEmpty then
                    (tokens, xs, Value(syntaxStack.head.nullable.get)::resultStack)
                else
                    val (t::ts) = tokens
                    val tKind = getKind(t)
                    if x.first.contains(tKind) then
                        x match
                            case Success(v) => 
                                (tokens,xs,Value(v)::resultStack)

                            case Failure() => 
                                throw Error(s"Parsing failed with token $t")

                            case Elem(k) => 
                                (ts,xs,Value(t)::resultStack)

                            case Transform(s, f) => 
                                (
                                    tokens,
                                    s :: xs,
                                    PartialTransform(f.asInstanceOf[Any => Any]):: resultStack
                                )
                            case Sequence(l,r) => 
                                (
                                    tokens,
                                    l :: r :: xs,
                                    EmptySequence::resultStack
                                )
                            case Disjunction(l,r) => 
                                if l.first.contains(getKind(t)) then
                                    (tokens, l :: xs, resultStack)
                                else
                                    (tokens, r :: xs, resultStack)
                            case Recursive(s,_) =>
                                (tokens, s::xs, resultStack)
                    else if x.isNullable then
                        (
                            tokens,
                            xs,
                            Value(x.nullable.get) :: resultStack
                        )
                    else
                        throw Error(s"Parsing failed with token $t")

            next(newTokens, newSyntaxStack, reduce(newResultStack))

    def reduce(result: Stack[ResultStackElement]): Stack[ResultStackElement] =
        import ResultStackElement._
        result match 
            case Value(v2) :: Value(v1) :: EmptySequence :: rest => reduce(Value((v1,v2)) :: rest)
            case Value(v) :: PartialTransform(f) :: rest => reduce(Value(f(v)) :: rest)
            case _ => result

    enum ResultStackElement:
        case Value(value: Any)
        case EmptySequence
        case PartialTransform(f: Any => Any)

    enum ParsingResult[A]:
        case ParsingSuccess[A](result: A) extends ParsingResult[A]
        case ParsingSuccessWithRest[A](result:A, rest: List[Token]) extends ParsingResult[A]
        case ParsingFailure[A](msg: String) extends ParsingResult[A]
        case HasConflict[A](msg: String) extends ParsingResult[A]