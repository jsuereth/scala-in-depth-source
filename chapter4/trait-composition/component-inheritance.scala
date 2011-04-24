/** Defines a base logging trait that classes can use to log status messages. */
trait Logger {
  def log(category : String, msg : String) : Unit = {
       println(msg)
  }
}
/** A Logger that will send log messages over a remote socket (not implemented) */
trait RemoteLogger extends Logger {
  //val socket = ..
  override def log(category : String, msg : String) : Unit = {
    //Send over socket
  }
}

/** A Logger that does nothing. */
trait NullLogger extends Logger {
   override def log(category : String, msg : String) : Unit = {}
}

/** This trait can be extended by class that use the Logger service.  This allows
 * those classes to be 'configured' via trait mixins with an appropriate logger at instantiation.
 */
trait HasLogger {
  val logger : Logger = new Logger {}
}

/**
 * This is a service for accessing data using query strings.   This service
 * extends the HasLogger class and can be configured when instantiated.
 */
trait DataAccess extends HasLogger {

   def query[A](in : String) : A = {
     logger.log("QUERY", in)     
     null.asInstanceOf[A]
   }
}

/** This trait can be mixed into to any HasLogger class to give them a remote logger. */
trait HasRemoteLogger extends HasLogger {
  override val logger : Logger = new RemoteLogger {}
}

/** This trait can be mixed into any HasLogger class to turn off logging. */
trait HasNullLogger extends HasLogger {
  override val logger : Logger = new NullLogger {}
}


// Foo is an instance of DataAccess configured with the logger we'd like to use.
object foo extends DataAccess with HasNullLogger {}

// This is a mechanism of defining a nested service such that pieces are still configurable.
trait HasDataAccess extends HasLogger {
  
}
