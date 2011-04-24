
trait Logger {
  def log(category : String, msg : String) : Unit = {
       println(msg)
  }
}



trait DataAccess {
   def query[A](in : String) : A = {
     5
   }
}


trait LoggedDataAccess extends DataAccess with Logger {
   def query[A](in : String) : A = {
      log("QUERY", in)
      super.query(in)
   }
}
