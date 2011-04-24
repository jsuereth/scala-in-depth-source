

trait Logger {
  def log(category : String, msg : String) : Unit = {
       println(msg)
  }
}

trait DataAccess with Logger {

   def query[A](in : String) : A = {
     log("QUERY", in)     
     5
   }
}
