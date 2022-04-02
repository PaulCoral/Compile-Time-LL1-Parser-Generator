package ll1compiletime.syntax

import ll1compiletime.parser.PartialParsingTable

/**
 * The trait to define a syntax executed at compile time
 * 
 * @tparam A the return type of the entry point
 * @tparam T the Token type
 * @tparam K the Kind type
 */
trait CompileTime[A,T,K]  {
    self: SyntaxDefinition[T,K] => 

    type Token = T

    type Kind = K

    /**
     * Type of a syntax in this context of [[CompileTime]]
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


    /**
     * The top level syntax, where the parsing begin.
     */
    lazy val entryPoint: CSyntax[A]

    /**
     * the macro call for the compile time analysis 
     * and construction of the syntax
     * 
     * @note should abslutely be an inline
     */
    inline def macroCall: PartialParsingTable[Kind]

    /**
     * Return a parsing table corresponding to the defined syntax.
     */
    final inline def parser = macroCall.withFunctionTable(this)
}