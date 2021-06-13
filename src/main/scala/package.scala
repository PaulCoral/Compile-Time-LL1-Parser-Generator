/**
 * This package provide a library based on
 * [Scall1on](https://github.com/epfl-lara/scallion)
 * to build LL1 parser, which syntax analysis and (most of) the parser 
 * construction can be done at compile time using macros.
 * 
 * The library itself does not contain compile time code, but it is designed
 * to be used with Scala 3 macros without any issue.
 * 
 * To use it you should put the following things in **A SINGLE FILE**, so
 * that it can be used with macros
 * 
 * - A macro call (inlined macro call + macro definition itself) 
 * with a call to [[ll1compiletime.buildParsingTable]] with the syntax
 * definition created in the following point
 * - The syntax definition in an object exteded with the SyntayDefintion trait
 * 
 * @example {
 *  inline def getPartialParsingTable = ${init}
 *  def init(using Quotes) = {
 *      Expr(buildParsingTable(SyntaxDef))
 *  }
 *  object SyntaxDef extends SyntaxDefinition[Int,MyToken,MyKind] {
 *      // syntax defintion
 *  }
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