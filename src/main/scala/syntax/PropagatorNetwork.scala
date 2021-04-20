package syntax

import scala.quoted._
import scala.collection.mutable.{Map,Set}
import TokensAndKinds.Kind

    
inline def propagate[A](inline syntax: Syntax[A])(using Quotes) = ${ init[A]('syntax) }

def init[A](syntax: Expr[Syntax[A]])(using Type[A])(using Quotes) = {
    import quotes.reflect.*
    val toBeUpdated = Set[Long]()
    val idToProp = Map[Long, Properties[A]]()
    val childToParent = Map[Long, Long]()
    //val content = Map[Long,Any]()
    def build[A](s: Expr[Syntax[A]]):Unit =
        s match {
            case '{Success[A](${v},${i})} => {
                val id = i.valueOrError
                toBeUpdated.add(id)
                idToProp.put(
                    id,
                    Properties(nullable = Some(v))
                )
            }
            
            case '{Failure[A]($i)} => 
                val id = i.valueOrError
                toBeUpdated.add(id)
                idToProp.put(
                    id,
                    Properties()
                )

            case '{Elem($k,$i)} => 
                val id = i.valueOrError
                toBeUpdated.add(id)
                val kindId = '{${k}.id}.valueOrError
                idToProp.put(
                    id,
                    Properties(first = Set(kindId))
                )

            case '{Transform($inner, $f, $i)} => 
                val id = i.valueOrError
                idToProp.put(
                    id,
                    Properties()
                )
                build(inner)
            
            case '{Sequence($left,$right,$i)} => 
                val id = i.valueOrError
                val leftId = '{$left.id}.valueOrError
                val rightId = '{$right.id}.valueOrError
                childToParent.put(leftId,id)
                childToParent.put(rightId,id)
                idToProp.put(
                    id,
                    Properties()
                )

            case '{Disjunction($left,$right,$i)} => 
                val id = i.valueOrError
                val leftId = '{$left.id}.valueOrError
                val rightId = '{$right.id}.valueOrError
                childToParent.put(leftId,id)
                childToParent.put(rightId,id)
                idToProp.put(
                    id,
                    Properties()
                )
        }
    ???
}


private class Properties[A](
    val first:Set[Long] = Set(),
    val snf: Set[Long] = Set(),
    var nullable: Option[Expr[A]] = None,
    var hasConflict: Boolean = false,
    var isProductive: Boolean = true
)

