package parser

import syntax._

import scala.collection.mutable.{Set,Map}
import scala.annotation.tailrec

object Parsing {
    type Kind = syntax.TokensAndKinds.Kind
    type Token = syntax.TokensAndKinds.Token

    private val ready: Set[Int] = Set()
    private val idToProperties: Map[Int, Properties] = Map()
    private val childToParent: Map[Int, Int] = Map()
    private val dependencies: Map[Int, Set[Int]] = Map()

    def apply[A](s: Syntax[A]) = 
        ready.clear
        idToProperties.clear
        childToParent.clear
        setUp(s.asInstanceOf[Syntax[Any]])
        //throw Exception()
        propagate()
        idToProperties

    private def setUp(s: Syntax[Any]):Unit = 
        val prop = Properties(s)
        idToProperties.put(s.id, prop)
        s match {
            case Success(v) => 
                prop.nullable = Some(v)
                ready.add(s.id)
            
            case Failure() =>
                prop.isProductive = false
                prop.hasConflict = true
                ready.add(s.id)

            case Elem(k) => 
                prop.first.add(k)
                ready.add(s.id)

            case Transform(inner,f) => 
                childToParent.put(inner.id,s.id)
                dependencies.put(s.id,Set(inner.id))
                prop.transform = Some(f.asInstanceOf[(Any) => Any])
                setUp(inner.asInstanceOf[Syntax[Any]]) // TODO tailrec

            case Disjunction(left, right) =>
                childToParent.put(left.id,s.id)
                childToParent.put(right.id,s.id)
                dependencies.put(s.id,Set(left.id,right.id))

                setUp(left) // TODO tailrec
                setUp(right) // TODO tailrec

            case Sequence(left,right) =>
                childToParent.put(left.id,s.id)
                childToParent.put(right.id,s.id)
                dependencies.put(s.id,Set(left.id,right.id))
                setUp(left.asInstanceOf[Syntax[Any]]) // TODO tailrec
                setUp(right.asInstanceOf[Syntax[Any]]) // TODO tailrec

            case Recursive(inner) => 
                if(!(idToProperties.contains(s.id))) then
                    childToParent.put(inner.id,s.id)
                    dependencies.put(s.id,Set(inner.id))
                    setUp(inner) // TODO tailrec

        }

    def propagate() = {
        while(ready.nonEmpty){
            ready.foreach{ (id) => 
                ready.remove(id)
                updateProperties(id)
                childToParent.get(id) match
                    case Some(parentId) => {
                        val dep = dependencies(parentId)
                        dep.remove(id)
                        if (dep.isEmpty){
                            // parent as no more dependencies => READY
                            ready.add(parentId)
                        }
                    }
                    case None => ()
            }
        }
    }

    def updateProperties(id: Int):Unit = 
        idToProperties.get(id) match {
            case None => ()
            case Some(prop@Properties(s)) =>
                s match {
                    case Success(v) => ()
                
                    case Failure() => ()

                    case Elem(k) => ()

                    case Transform(inner,f) =>
                        val child = idToProperties(inner.id)
                        // Productive
                        prop.isProductive = child.isProductive
                        // First
                        prop.first.addAll(child.first)
                        // Nullable
                        prop.nullable = child.nullable
                        // Should-Not-Follow
                        prop.snf.addAll(child.snf)
                        // Conflict
                        prop.hasConflict = child.hasConflict

                    case Disjunction(left, right) =>
                        val lp = idToProperties(left.id)
                        val rp = idToProperties(right.id)
                        // Productive
                        prop.isProductive = lp.isProductive || rp.isProductive
                        // First
                        prop.first.addAll(lp.first)
                        prop.first.addAll(rp.first)
                        // Nullable
                        if(lp.isNullable){
                            prop.nullable = lp.nullable
                        }else{
                            prop.nullable = rp.nullable
                        }
                        // Should-Not-Follow
                        prop.snf.addAll(lp.snf)
                        prop.snf.addAll(rp.snf)
                        if(lp.isNullable){
                            prop.snf.addAll(rp.first)
                        }
                        if(rp.isNullable){
                            prop.snf.addAll(lp.first)
                        }
                        // Conflict
                        val both = lp.isNullable && rp.isNullable
                        val has = lp.hasConflict || rp.hasConflict
                        val intersect = lp.first.intersect(rp.first)
                        val ff = intersect.nonEmpty
                        prop.hasConflict = both || has || ff
                        if(both){
                            throw LL1Conflict.NullableNullable("")
                        }
                        if(ff){
                            throw LL1Conflict.FirstFirst(s"${printSetContent(intersect)}")
                        }


                    case Sequence(left,right) =>
                        val lp = idToProperties(left.id)
                        val rp = idToProperties(right.id)
                        // Productive
                        prop.isProductive = lp.isProductive && rp.isProductive
                        // First
                        if(rp.isProductive){
                            prop.first.addAll(lp.first)
                        }
                        if(lp.isNullable){
                            prop.first.addAll(rp.first)
                        }
                        // Nullable
                        if(lp.isNullable && rp.isNullable){
                            prop.nullable = Some((lp.nullable.get,rp.nullable.get))
                        }
                        // Should-Not-Follow
                        if(rp.isNullable){
                            prop.snf.addAll(lp.snf)
                        }
                        if(lp.isProductive){
                            prop.snf.addAll(rp.snf)
                        }
                        // Conflict
                        val has = lp.hasConflict || rp.hasConflict
                        val intersect = lp.snf.intersect(rp.first)
                        val snfFirst = intersect.nonEmpty
                        prop.hasConflict = has || snfFirst
                        if(snfFirst){
                            throw LL1Conflict.SNFFirst(s"${printSetContent(intersect)}")
                        }


                    case Recursive(inner) => 
                        val child = idToProperties(inner.id)
                        // Productive
                        prop.isProductive = child.isProductive
                        // First
                        prop.first.addAll(child.first)
                        // Nullable
                        prop.nullable = child.nullable
                        // Should-Not-Follow
                        prop.snf.addAll(child.snf)
                        // Conflict
                        prop.hasConflict = child.hasConflict
                }
        }

    def printSetContent(set: Set[?]): String = {
        set.foldLeft("")((str,elem) => str + s"$elem,")
    }

    case class Properties(val syntax: Syntax[Any]){
        val first: Set[Kind] = Set()
        val snf: Set[Kind] = Set()
        var transform: Option[(Any) => Any] = None
        var nullable:Option[Any] = None
        var isProductive:Boolean = true
        var hasConflict = false

        def isNullable = nullable.nonEmpty
    }

    enum LL1Conflict(msg: String) extends Exception(msg) {
        case NullableNullable(msg: String) extends LL1Conflict(s"Two branches of a disjunction are nullable $msg")
        case FirstFirst(msg: String) extends LL1Conflict(s"Two branches of a disjunction have non disjoint first set : $msg")
        case SNFFirst(msg: String) extends LL1Conflict(s"The should-not-follow set of the left-hand side of a sequence and the first set of the right-hand side of that sequence both contain the same token kind: $msg")
    }
}