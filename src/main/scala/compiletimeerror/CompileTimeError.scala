package compiletimeerror

/**
 * Currently a prototype for throwing compile time error
 */
object CompileTimeError {
    import scala.quoted._

    /**
     * Error message that will be thrown at compile time if debug is on and 
     * if argument are available at compile time
     */
    inline def compileTimeError(inline debug : Boolean, inline msg: String = "") = 
        ${ cteMacro('debug, 'msg) }

    private def cteMacro(debug : Expr[Boolean], msg: Expr[String])(using Quotes): Expr[Unit] = 
        if(debug.value.getOrElse(false)){
            val formattedMsg = msg.value.getOrElse("No error message given at compile time.")
            throw Error(s"An error happened during compile time. $formattedMsg")
        }
        '{()}
}
