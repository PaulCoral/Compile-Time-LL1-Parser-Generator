package ll1compiletime 
    
/**
 * The trait to define a syntax.
 * See the operators here [[ll1compiletime.syntax.SyntaxDefinition]]
 * 
 * @note this type definition is so that be have only to import
 * `ll1compiletime._`
 * 
 * @tparam A the return type of the entry point
 * @tparam T the Token type
 * @tparam K the Kind type
 */
type SyntaxDefinition[A,T,K] = ll1compiletime.syntax.SyntaxDefinition[A,T,K]

/**
 * Define a syntax that will be analyzed at compile time
 * 
 * @see [[ll1compiletime.syntax.SyntaxDefinition]] for detail
 */
type CompileTime[A,T,K] = ll1compiletime.syntax.CompileTime[A,T,K]

/**
 * A pair of value
 * 
 * Used as infix type
 * 
 * @example {{{
 *  val s: CSyntax[A ~ B] = ???
 *  s.map { 
 *      case a ~ b => ??? 
 *  } 
 * }}}
 * 
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
 * @tparam K the Token Kind used in the parser
 * @param sd the syntax definition
 * @return the partial parsing table created by the Parsing object
 */
def buildParsingTable[K](parser: CompileTime[?,?,K]) = ll1compiletime.parser.Parsing(parser)
