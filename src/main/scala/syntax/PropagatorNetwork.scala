package syntax

import scala.quoted._
import scala.collection.mutable.{Map,Set}

    
transparent inline def propagate[A](inline syntax: Syntax[A])(using Quotes) = ${ init[A]('syntax) }

def init[A](syntax: Expr[Syntax[A]])(using Type[A])(using Quotes) = {
    import quotes.reflect.*
    val starters = Set[Long]()
    val idToProp = Map[Long, Properties[A]]()
    val childToParent = Map[Long, Long]()
    val content = Map[Long,Any]()

    syntax match
        case '{Success(${x})} => ???
    ???
}


private class Properties[A] {
    val first:Set[Kind] = Set()
    val snf: Set[Kind] = Set()
    var nullable: Option[A] = None
    var hasConflict: Boolean = false
    var isProductive: Boolean = true
}

