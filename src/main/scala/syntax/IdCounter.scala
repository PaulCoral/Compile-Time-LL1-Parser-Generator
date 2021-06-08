package syntax

class IdCounter {
    private var id = 0

    def nextId:Int = {
        val prev: Int = id
        id = id + 1
        prev
    }
}