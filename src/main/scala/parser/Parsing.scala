package parser

import syntax._

import scala.collection.mutable.{Set,Map}
import scala.annotation.tailrec

object Parsing {
    type Kind = syntax.TokensAndKinds.Kind
    type Token = syntax.TokensAndKinds.Token

    private val ready: Set[Int] = Set()
    private val idToProperties: Map[Int, Properties] = Map()
    private val childToParent: Map[Int, Set[Int]] = Map()
    private val dependencies: Map[Int, Set[Int]] = Map()

    // Parsing
    private val table: Map[(Int,Kind),ParsingTableInstruction] = Map()
    private val nullable: Map[Int,Any] = Map()

    def apply[A](s: Syntax[A]) = {        
        cleaning
        setUp(s.asInstanceOf[Syntax[Any]])
        propagate()
        idToProperties
    }

    private def cleaning = {
        ready.clear
        idToProperties.clear
        childToParent.clear
        dependencies.clear
        table.clear
        nullable.clear
    }

    private def addChildToParent(child:Int, parent: Int) = {
        childToParent.get(child) match {
            case None => childToParent.put(child, Set(parent))
            case Some(set) => set.add(parent)
        }
    }

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
                addChildToParent(inner.id,s.id)
                dependencies.put(s.id,Set(inner.id))
                prop.transform = Some(f.asInstanceOf[(Any) => Any])
                setUp(inner.asInstanceOf[Syntax[Any]]) // TODO tailrec

            case Disjunction(left, right) =>
                addChildToParent(left.id,s.id)
                addChildToParent(right.id,s.id)
                dependencies.put(s.id,Set(left.id,right.id))

                setUp(left) // TODO tailrec
                setUp(right) // TODO tailrec

            case Sequence(left,right) =>
                addChildToParent(left.id,s.id)
                addChildToParent(right.id,s.id)
                dependencies.put(s.id,Set(left.id,right.id))
                setUp(left.asInstanceOf[Syntax[Any]]) // TODO tailrec
                setUp(right.asInstanceOf[Syntax[Any]]) // TODO tailrec

            case Recursive(inner) => 
                if(!(dependencies.contains(s.id))) then
                    addChildToParent(inner.id,s.id)
                    dependencies.put(s.id,Set(inner.id))
                    setUp(inner) // TODO tailrec

            case _ => throw IllegalStateException(s"Unkown Syntax $s")

        }

    def propagate() = {
        while(ready.nonEmpty){
            ready.foreach{ (id) => 
                ready.remove(id)
                updateProperties(id)
                childToParent.get(id) match
                    case Some(parentSet) => {
                        parentSet.foreach{ parentId => 
                            val dep = dependencies(parentId)
                            dep.remove(id)
                            if (dep.isEmpty){
                                // parent as no more dependencies => READY
                                ready.add(parentId)
                            }
                        }
                    }
                    case None => ()
            }
        }
    }

    def updateProperties(id: Int):Unit = {
        import ParsingTableContext._
        import ParsingTableInstruction._

        idToProperties.get(id) match {
            case None => ()
            case Some(prop@Properties(s)) =>
                s match {
                    case Success(v) => 
                        // Nullable Table
                        nullable.put(s.id,v)

                    case Failure() => ()

                    case Elem(k) => 
                        // Parsing Table
                        table.put((s.id,k), Terminal)

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

                        // Parsing Table
                        child.first.foreach { k =>
                            table.put((s.id,k), NonTerminal(inner.id, ApplyF(f.asInstanceOf[Any => Any])))
                        }

                        // Nullable Table
                        addToNullableTable(s.id, prop.nullable)
                            

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
                            throw LL1Conflict.NullableNullable()
                        }
                        if(ff){
                            throw LL1Conflict.FirstFirst(intersect)
                        }

                        // Parsing Table
                        lp.first.foreach { k =>
                            table.put((s.id,k), NonTerminal(left.id, Nothing))
                        }
                        rp.first.foreach { k =>
                            table.put((s.id,k), NonTerminal(right.id, Nothing))
                        }

                        // Nullable Table
                        addToNullableTable(s.id, prop.nullable)


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
                            throw LL1Conflict.SNFFirst(intersect)
                        }

                        // Parsing Table
                        lp.first.foreach { k =>
                            table.put((s.id,k), NonTerminal(left.id, FollowedBy(right.id)))
                        }
                        lp.nullable match {
                            case None => ()
                            case Some(v) =>
                                rp.first.foreach { k =>
                                    table.put((s.id,k), NonTerminal(right.id, PrependedBy(v)))
                                }
                        }

                        // Nullable Table
                        addToNullableTable(s.id, prop.nullable)


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

                        // Parsing Table
                        child.first.foreach { k =>
                            table.put((s.id,k), NonTerminal(inner.id, Nothing))
                        }

                        // Nullable Table
                        addToNullableTable(s.id, prop.nullable)

                    case _ => throw IllegalStateException(s"Unkown Syntax $s")
                }
            }
        }

    private def addToNullableTable(id:Int, opt : Option[Any]) = {
        opt match {
            case None => ()
            case Some(v) => nullable.put(id, v)
        }
    }

    private def printSetContent(set: Set[?]): String = {
        if set.isEmpty then
            "<None>"
        else
            val h = set.head
            val t = set.tail
            t.foldLeft(s"$h")((str,elem) => str + s", $elem")
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

    enum ParsingTableInstruction {
        case NonTerminal(id: Int, elem: ParsingTableContext) extends ParsingTableInstruction
        case Terminal extends ParsingTableInstruction
    }

    enum ParsingTableContext {
        case ApplyF(f: (Any) => Any) extends ParsingTableContext
        case PrependedBy(v: Any) extends ParsingTableContext
        case FollowedBy(s: Int) extends ParsingTableContext
        case Nothing extends ParsingTableContext
    }

    enum LL1Conflict(msg: String) extends Exception(msg) {
        case NullableNullable() extends LL1Conflict(s"Nullable Conflict : Two branches of a disjunction are nullable")
        case FirstFirst(kind: Set[Kind]) extends LL1Conflict(s"First-First Conflict : Two branches of a disjunction have non disjoint first sets : ${printSetContent(kind)}")
        case SNFFirst(kind: Set[Kind]) extends LL1Conflict(s"First-Follow Conflict : The should-not-follow set of the left-hand side of a sequence and the first set of the right-hand side of that sequence are not disjoint: ${printSetContent(kind)}")

        override def toString = s"\n⚠️ $msg ⚠️\n"
    }
}