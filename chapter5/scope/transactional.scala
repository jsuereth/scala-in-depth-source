
trait Transaction



object DefaultTransaction extends Transaction {
  override def toString = "DefaultTransaction"
}


trait DatabaseAction {

  def executeQuery(query : String)(implicit transaction : Transaction) : Unit = {
    println("Executing [" + query + "] on " + transaction)
  }

  def doAction(implicit transaction : Transaction) : Unit
}

object TestAction extends DatabaseAction {
  override def doAction(implicit transaction : Transaction) {
    executeQuery("INSERT MY DATA")
  }
}