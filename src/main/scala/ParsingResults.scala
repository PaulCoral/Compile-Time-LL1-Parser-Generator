package ll1compiletime

/**
 * the result of parsing
 */
sealed trait ParsingResult[A] {
    /**
     * Cast the old type `A` into the new type `B`
     * 
     * @tparam B the new type to cast to
     */
    def toType[B]:ParsingResult[B]
}
/**
 * A sucessful parsing result, all tokens have been consumed
 * 
 * @param v the value of produced by the parser
 */
case class ParsedSuccessfully[A](v: A) extends ParsingResult[A] {
    /**
     * Cast the old type `A` into the new type `B`
     * 
     * @tparam B the new type to cast to
     */
    def toType[B]:ParsingResult[B] = ParsedSuccessfully(v.asInstanceOf[B])

    override def toString = s"Successful parsing: $v"
}
/**
 * A sucessful parsing result, but **NOT** all tokens have been consumed
 * 
 * @param v the value of produced by the parser
 * @param tokens the remaining Tokens
 */
case class ParsedSuccessfullyWithRest[A,Token](v: A, tokens:List[Token]) extends ParsingResult[A]{
    /**
     * Cast the old type `A` into the new type `B`
     * 
     * @tparam B the new type to cast to
     */
    def toType[B]:ParsingResult[B] = ParsedSuccessfullyWithRest(v.asInstanceOf[B],tokens)

    override def toString = s"Successful parsing with rest. Value : $v\nRest: $tokens"
}
/**
 * A failed parsing result, the end of the Token sequence have been
 * reached before the end of the parsing.
 * 
 * @param expected the expected following Kinds of Token
 */
case class UnexpectedEnd[A,Kind](expected: Set[Kind]) extends ParsingResult[A]{
    /**
     * Cast the old type `A` into the new type `B`
     * 
     * @tparam B the new type to cast to
     */
    def toType[B]:ParsingResult[B] = UnexpectedEnd[B,Kind](expected)

    override def toString = s"Unexpected End, expected kind : $expected"
}
/**
 * A failed parsing result, An unexpected token has been encountered during
 * during parsing
 * 
 * @param expected the expected following Kinds of Token
 */
case class UnexpectedToken[A,Kind](k: Kind, expected: Set[Kind]) extends ParsingResult[A]{
    /**
     * Cast the old type `A` into the new type `B`
     * 
     * @tparam B the new type to cast to
     */
    def toType[B]:ParsingResult[B] = UnexpectedToken[B,Kind](k,expected)

    override def toString = s"Unexpected Token of Kind $k, expected kind : $expected"
}