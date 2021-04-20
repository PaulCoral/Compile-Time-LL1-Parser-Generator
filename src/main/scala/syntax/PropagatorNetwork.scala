package syntax

import scala.quoted._
import scala.collection.mutable.{Map,Set}
import TokensAndKinds.Kind

    
inline def propagate(inline s: Syntax[Any]) = ${ init('s) }

def init(s: Expr[Syntax[Any]])(using Quotes):Expr[Unit] = {
    val toBeUpdated = Set[Long]()
    val idToProp = Map[Long, Properties[Any]]()
    val childToParent = Map[Long, Long]()

    object IdsGenerator{
        private var id: Long = 0
        def next = 
            val prec = id
            id = id + 1
            prec
    }

    def buildId(s: Expr[Syntax[Any]])(using Quotes):Long =
        if('{${s}.hasId}.valueOrError) {
            '{${s}.getId}.valueOrError.get
        }else{
            val id = Expr(IdsGenerator.next)
            '{${s}.setId($id)}
            id.valueOrError
        }

    def build(s: Expr[Syntax[Any]])(using Quotes):Unit =
        import quotes.reflect.*
        
        val id = buildId(s)
        s match {
            case '{Success(${v})} => {
                toBeUpdated.add(id)
                idToProp.put(
                    id,
                    Properties(nullable = Some(v))
                )
            }
            
            case '{Failure()} => 
                toBeUpdated.add(id)
                idToProp.put(
                    id,
                    Properties()
                )

            case '{Elem($k)} => 
                toBeUpdated.add(id)
                val kindId = '{${k}.id}.valueOrError
                idToProp.put(
                    id,
                    Properties(first = Set(kindId))
                )

            case '{Transform($inner, $f)} => 
                childToParent.put(buildId(inner),id)
                idToProp.put(
                    id,
                    Properties()
                )
                build(inner)
            
            case '{Sequence($left,$right)} =>
                val leftId = buildId(left)
                val rightId = buildId(left)
                childToParent.put(leftId,id)
                childToParent.put(rightId,id)
                idToProp.put(
                    id,
                    Properties()
                )
                build(left)
                build(right)

            case '{Disjunction($left,$right)} => 
                val leftId = buildId(left)
                val rightId = buildId(left)
                childToParent.put(leftId,id)
                childToParent.put(rightId,id)
                idToProp.put(
                    id,
                    Properties()
                )

            case '{Recursive($inner)} =>
                if(!idToProp.contains(id)){
                    val innerId = buildId(inner)
                    childToParent.put(innerId,id)
                    idToProp.put(
                        id,
                        Properties()
                    )
                }
        }
    build(s)
    '{()}
}


private class Properties[A](
    val first:Set[Long] = Set(),
    val snf: Set[Long] = Set(),
    var nullable: Option[Expr[A]] = None,
    var hasConflict: Boolean = false,
    var isProductive: Boolean = true
)

