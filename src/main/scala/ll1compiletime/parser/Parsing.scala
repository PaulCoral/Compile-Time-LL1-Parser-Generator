package ll1compiletime.parser

import ll1compiletime.syntax._

import ParsingTable.ParsingTableContext
import ParsingTable.SymboleType
import ParsingTable.ParsingTableContext._
import ParsingTable.SymboleType._
import ParsingTable.{Nullable,Leaf,Node,TransNullable}
import Parsing._

import scala.quoted._
import scala.collection.mutable.{Set,Map,Queue}
import scala.annotation.tailrec

private[ll1compiletime] class Parsing[Kind] {
    private val ready = Queue[Int]()
    private val idToProperties: Map[Int, Properties[Kind]] = Map()
    private val childToParent: Map[Int, Set[Int]] = Map()
    private val conflicts: Set[LL1Conflict] = Set()
    private val table: Map[(Int,Kind),SymboleType] = Map()
    private val nullable: Map[Int,Nullable] = Map()

    /**
     * Returns a PartialParsingTable from a SyntaxDefinition
     */
    def apply(sd: CompileTime[?,?,Kind]):PartialParsingTable[Kind] = {        
        val s = sd.entryPoint
        cleaning
        setUp(s.asInstanceOf[Syntax[Any,?,Kind]])
        propagate
        if(conflicts.nonEmpty){
            throw Exception(conflicts.toString)
        }
        new PartialParsingTable[Kind](s.id, table.toMap, nullable.toMap)
    }

    /**
     * Clear all table used
     */
    private def cleaning = {
        ready.clear
        idToProperties.clear
        childToParent.clear
        conflicts.clear
        table.clear
        nullable.clear
    }

    /**
     * Add a parent to the parent set corresponding to the given child
     */
    private def addChildToParent(child:Int, parent: Int) = {
        childToParent.get(child) match {
            case None => childToParent.put(child, Set(parent))
            case Some(set) => set.add(parent)
        }
    }

    /**
     * Set up the tables from the entry syntax
     */
    private def setUp(entry: Syntax[Any,?,Kind]):Unit = {
        val visited = Set[Int]()
        val queue = Queue[Syntax[Any,?,Kind]](entry)

        while(queue.nonEmpty){
            val s = queue.dequeue
            
            if(!(visited contains s.id)){
                visited += s.id
                val prop = Properties(s)
                idToProperties.put(s.id, prop)
                s match {
                    case Success(v) => 
                        prop.nullable = Some(Leaf(s.id))
                        prop.update(true)
                        ready.enqueue(s.id)
                    
                    case Failure() =>
                        prop.isProductive = false
                        ready.enqueue(s.id)
                        prop.update(true)

                    case Elem(k:Kind) => 
                        prop.first.add(k)
                        ready.enqueue(s.id)
                        prop.update(true)

                    case Transform(inner,f) => 
                        addChildToParent(inner.id,s.id)
                        queue.enqueue(inner.asInstanceOf[Syntax[Any,?,Kind]]) 

                    case Disjunction(left, right) =>
                        addChildToParent(left.id,s.id)
                        addChildToParent(right.id,s.id)
                        queue.enqueue(left) 
                        queue.enqueue(right)

                    case Sequence(left,right) =>
                        addChildToParent(left.id,s.id)
                        addChildToParent(right.id,s.id)
                        queue.enqueue(left.asInstanceOf[Syntax[Any,?,Kind]]) 
                        queue.enqueue(right.asInstanceOf[Syntax[Any,?,Kind]]) 

                    case Recursive(inner) => 
                        addChildToParent(inner.id,s.id)
                        queue.enqueue(inner)

                    case _ => throw IllegalArgumentException(s"Unkown Syntax $s")
                }
            }
        }
    }


    /**
     * Propagate the changes after the SetUp, until there's
     * nothing more to update
     */
    private def propagate = {
        while(ready.nonEmpty){
            val id = ready.dequeue
            updateProperties(id)
            idToProperties.get(id) match {
                case None => ()
                case Some(prop) =>
                    if (prop.updated){
                        prop.updated = false
                        childToParent.get(id) match {
                            case Some(parentSet) => 
                                parentSet.foreach{ parentId => 
                                    ready.enqueue(parentId)
                                }
                            
                            case None => ()
                        }
                    }
            }
            
        }
    }

    /**
     * Update the property corresponding to the given
     * Syntax id
     * 
     * @param id the syntax id
     */
    def updateProperties(id: Int):Unit = {
        idToProperties.get(id) match {
            case None => ()
            case Some(prop@Properties(s)) =>
                s match {
                    case Success(v) => ()

                    case Failure() => ()

                    case Elem(k) => 
                        // Parsing Table
                        table.put((s.id,k), Terminal)

                    case Transform(inner,_) =>
                        val child = idToProperties(inner.id)
                        // Productive
                        prop.isProductive = child.isProductive
                        // First
                        prop.first.addAll(child.first)
                        // Nullable
                        prop.nullable = child.nullable.map{ i =>
                            TransNullable(i,s.id)
                        }

                        // Should-Not-Follow
                        prop.snf.addAll(child.snf)
                        // Conflict
                        prop.hasConflict = child.hasConflict

                        // updated if child is updated
                        prop.updated = true

                        // Parsing Table
                        child.first.foreach { k =>
                            table.put((s.id,k), symboleType(inner.id,k,ApplyF(s.id)))
                        }
                            

                    case Disjunction(left, right) =>
                        val lp = idToProperties(left.id)
                        val rp = idToProperties(right.id)

                        // Productive
                        prop.isProductive = lp.isProductive || rp.isProductive

                        // First
                        prop.update(prop.first.addAllAndNotify(lp.first))
                        prop.update(prop.first.addAllAndNotify(rp.first))

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
                            prop.update(prop.snf.addAllAndNotify(rp.first))
                        }
                        if(rp.isNullable){
                            prop.update(prop.snf.addAllAndNotify(lp.first))
                        }

                        // Conflict
                        val both = lp.isNullable && rp.isNullable
                        val has = lp.hasConflict || rp.hasConflict
                        val intersect = lp.first.intersect(rp.first)
                        val ff = intersect.nonEmpty
                        prop.hasConflict = both || has || ff
                        if(both){
                            conflicts.add(LL1Conflict.NullableNullable())
                        }
                        if(ff){
                            conflicts.add(LL1Conflict.FirstFirst(intersect))
                        }

                        // Parsing Table
                        lp.first.foreach { k =>
                            table.put((s.id,k), table((left.id,k)))//NonTerminal(left.id, Passed))
                        }
                        rp.first.foreach { k =>
                            table.put((s.id,k), table((right.id,k)))//NonTerminal(right.id, Passed))
                        }


                    case Sequence(left,right) =>
                        val lp = idToProperties(left.id)
                        val rp = idToProperties(right.id)
                        // Productive
                        prop.isProductive = lp.isProductive && rp.isProductive
                        // First
                        if(rp.isProductive){
                            prop.update(prop.first.addAllAndNotify(lp.first))
                        }
                        if(lp.isNullable){
                            prop.update(prop.first.addAllAndNotify(rp.first))
                        }
                        // Nullable
                        if(lp.isNullable && rp.isNullable){
                            prop.nullable = Some(Node(lp.nullable.get,rp.nullable.get))
                        }
                        // Should-Not-Follow
                        if(rp.isNullable){
                            prop.update(prop.snf.addAllAndNotify(lp.snf))
                        }
                        if(lp.isProductive){
                            prop.update(prop.snf.addAllAndNotify(lp.snf))
                        }
                        // Conflict
                        val has = lp.hasConflict || rp.hasConflict
                        val intersect = lp.snf.intersect(rp.first)
                        val snfFirst = intersect.nonEmpty
                        prop.hasConflict = has || snfFirst
                        if(snfFirst){
                            conflicts.add(LL1Conflict.SNFFirst(intersect))
                        }

                        // Parsing Table
                        lp.first.foreach { k =>
                            table.put((s.id,k), symboleType(left.id,k ,FollowedBy(right.id)))
                        }
                        if (lp.isNullable) {
                            rp.first.foreach { k =>
                                table.put((s.id,k), symboleType(right.id, k,PrependedByNullable(lp.syntax.id)))
                            }
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

                        prop.updated = true

                        // Parsing Table
                        child.first.foreach { k =>
                            table.put((s.id,k), table((inner.id,k)))//NonTerminal(inner.id, Passed))
                        }

                    case _ => throw IllegalStateException(s"Unkown Syntax $s")
                }
                addToNullableTable(s.id, prop.nullable)
            }
        }

    /**
     * Add a nullable to the syntax id
     */
    private def addToNullableTable(id:Int, opt : Option[Nullable]) = {
        opt match {
            case None => ()
            case Some(v) => nullable.put(id, v)
        }
    }

    
    private def symboleType(
        ns: Int,
        k: Kind,
        c: ParsingTableContext
    ):SymboleType = table((ns,k)) match {
        case NonTerminal(i,cs) => NonTerminal(i,cs :+ c)
        case Terminal => NonTerminal(ns,List(c))
    }
}

private[ll1compiletime] object Parsing {
    /**
     * Apply the syntax definition to a new Parsing, and return the result
     * 
     * @param sd the syntax definition
     * @return the partial parsing table created by the Parsing object
     */
    def apply[K](sd: CompileTime[?,?,K]):PartialParsingTable[K] = {
        val parsing = new Parsing[K]
        parsing(sd)
    }

    /**
     * Properties of the given syntax
     */
    private case class Properties[Kind](syntax: Syntax[Any,?,Kind]){
        val first: Set[Kind] = Set()
        val snf: Set[Kind] = Set()
        var nullable:Option[Nullable] = None
        var isProductive:Boolean = true
        var hasConflict = false

        var updated = false

        def isNullable = nullable.nonEmpty

        def update(b: Boolean) = updated = updated || b
    }

    /**
     * Conflict that can arise during the construction of the
     * parsing table
     * 
     * @param msg an error message
     */
    enum LL1Conflict(msg: String) extends Exception(msg) {
        /** A nullable conflict */
        case NullableNullable() extends LL1Conflict(s"Nullable Conflict : Two branches of a disjunction are nullable")
        /**
         * First first conflict
         * 
         * @param kind the set of kind that are causing the conflict
         */
        case FirstFirst[Kind](kind: Set[Kind]) extends LL1Conflict(s"First-First Conflict : Two branches of a disjunction have non disjoint first sets : ${kind.printSetContent}")
        /**
         * First Follow conflict
         * 
         * @param kind the set of kind that are causing the conflict
         */
        case SNFFirst[Kind](kind: Set[Kind]) extends LL1Conflict(s"First-Follow Conflict : The should-not-follow set of the left-hand side of a sequence and the first set of the right-hand side of that sequence are not disjoint: ${kind.printSetContent}")

        override def toString = s"\n⚠️ $msg ⚠️\n"
    }

    extension [A](thiz:Set[A]){
        /**
         *  Add all item in `that` to `thiz`. 
         *  @param thiz set to add item to
         *  @param that set to take item from
         *  
         *  @return `true` if thiz has changed, `false` otherwise
         */
        private def addAllAndNotify(that:Set[A]):Boolean = {
            if(that.subsetOf(thiz)){
                false
            } else {
                thiz.addAll(that)
                true
            }
        }

        /**
         * Print the content of a set in a clearer way
         */
        private def printSetContent: String = {
        if thiz.isEmpty then
            "<None>"
        else
            val h = thiz.head
            val t = thiz.tail
            t.foldLeft(s"$h")((str,elem) => str + s", $elem")
        }
    }
}

