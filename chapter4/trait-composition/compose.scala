
trait Logger {
  def log(category : String, msg : String) : Unit = {
       println(msg)
  }
}



trait DataAccess {
   val logger = new Logger

   def query[A](in : String) : A = {
     logger.log("QUERY", in)     
     5
   }
}
