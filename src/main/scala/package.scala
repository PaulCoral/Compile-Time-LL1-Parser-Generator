/**
 * 
 */
package ll1compiletime {
    
    /**
     * The trait to define a syntax
     * 
     * @tparam A the return type of the entry point
     * @tparam T the Token type
     * @tparam K the Kind type
     */
    trait SyntaxDefinition[A,T,K] extends ll1compiletime.syntax.SyntaxDefinition[A,T,K]

    /**
     * A pair of value
     * 
     * Used as infix type
     * 
     * @example case a ~ b ~ c => ???
     * 
     * @param _1 first value
     * @param _2 second value
     */
    case class ~[+A, +B](_1: A, _2: B) {
        /**
         * Build a pair from this pair and a new value
         * 
         * @param next the value to append to this pair
         */
        def ~[C](next: C): (A ~ B) ~ C = new ~(this, next)
    }


    /**
     * Apply the syntax definition to a new Parsing, and return the result
     * 
     * @param sd the syntax definition
     * @return the partial parsing table created by the Parsing object
     */
    def buildParsingTable[K](parser: SyntaxDefinition[?,?,K]) = ll1compiletime.parser.Parsing(parser)
}