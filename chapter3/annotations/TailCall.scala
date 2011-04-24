import annotation.tailrec

case class Node(name : String, edges : Seq[Node] = Nil)

object BFS {
  def search(start : Node, p : Node => Boolean) = {
     @tailrec
     def loop(nodeQueue : List[Node], visited : Set[Node]) : Option[Node] = 
       nodeQueue match {
         case head :: tail if p(head) => 
           Some(head)
         case head :: tail if !visited.contains(head) =>
           loop(tail ++ head.edges, visited + head)
         case head :: tail =>
           loop(tail, visited)
         case Nil => 
           None
       }
     loop(List(start), Set())
  }
}
