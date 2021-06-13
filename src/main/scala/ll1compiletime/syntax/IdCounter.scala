package ll1compiletime.syntax

/**
 * A counter used to provide Ids
 */
private[ll1compiletime] class IdCounter {
    /** the internal counter */
    private var id = 0

    /**  
     * Return a new unique id
     * 
     * @return new unique id
     */    
    def nextId:Int = {
        val prev: Int = id
        id = id + 1
        prev
    }
}