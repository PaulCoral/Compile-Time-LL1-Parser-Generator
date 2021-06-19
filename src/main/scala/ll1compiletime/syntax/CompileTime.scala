package ll1compiletime.syntax

trait CompileTime[A,T,K] extends SyntaxDefinition[A,T,K] {
    /** a given IdCounter that gives unique ids to Syntaxes */
    final given idc:IdCounter = new IdCounter

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
}