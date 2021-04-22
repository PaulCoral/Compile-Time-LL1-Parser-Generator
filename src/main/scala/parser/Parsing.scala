package parser

import syntax._

import scala.collection.mutable.{Set,Map}
import scala.annotation.tailrec

object Parsing {
    type Kind = syntax.TokensAndKinds.Kind
    type Token = syntax.TokensAndKinds.Token

    private val starters: Set[Int] = Set()
    private val nextStarters: Set[Int] = Set()
    private val idToPropertes: Map[Int, Properties] = Map()
    private val childToParent: Map[Int, Int] = Map()


    def apply[A](s: Syntax[A]) = 
        starters.clear
        idToPropertes.clear
        childToParent.clear
        setUp(s.asInstanceOf[Syntax[Any]])

    private def setUp(s: Syntax[Any]):Unit = 
        val prop = Properties(s)
        idToPropertes.put(s.id, prop)
        s match {
            case Success(v) => 
                prop.nullable = Some(v)
                starters.add(s.id)
            
            case Failure() =>
                prop.isProductive = false
                prop.hasConflict = true
                starters.add(s.id)

            case Elem(k) => 
                prop.first.add(k)
                starters.add(s.id)

            case Transform(inner,f) => 
                childToParent.put(inner.id,s.id)
                prop.transform = Some(f.asInstanceOf[(Any) => Any])
                setUp(inner.asInstanceOf[Syntax[Any]]) // TODO tailrec

            case Disjunction(left, right) =>
                childToParent.put(left.id,s.id)
                childToParent.put(right.id,s.id)
                setUp(left) // TODO tailrec
                setUp(right) // TODO tailrec

            case Sequence(left,right) =>
                childToParent.put(left.id,s.id)
                childToParent.put(right.id,s.id)
                setUp(left.asInstanceOf[Syntax[Any]]) // TODO tailrec
                setUp(right.asInstanceOf[Syntax[Any]]) // TODO tailrec

            case Recursive(inner) => 
                if(!(idToPropertes.contains(s.id))) then
                    childToParent.put(inner.id,s.id)
                    setUp(inner) // TODO tailrec

        }

    def propagate() = {
        starters.foreach{ (id) => 
            childToParent.remove(id) match
                case Some(parentId) => {
                    nextStarters.add(parentId)
                    parentId
                }
                case None => ()
        }
    }

    def updateProperties(id: Int) = ???

    class Properties(val syntax: Syntax[Any]){
        val first: Set[Kind] = Set()
        val snf: Set[Kind] = Set()
        var transform: Option[(Any) => Any] = None
        var nullable:Option[Any] = None
        var isProductive:Boolean = true
        var hasConflict = false
    }
}