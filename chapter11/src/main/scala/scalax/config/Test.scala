package scalax.config.test


import scalax.functional.{Functor,Monad,Applicative,ApplicativeMagic}
import scalax.config.Config
import scalax.functional.Implicits._

class DataStore(connection: java.sql.Connection) {
  //
}

class WorkerPool() {}

class Application(ds: DataStore, pool: WorkerPool) {
  def doStuff = "Go Go Main Application!"
}

// This test shows how to configure a software application using the Config monad. 
object Test {

  // Default properties
  def properties = Config.propertyfile(new java.io.File("jdbc.properties"))
  // JDBC properties
  def jdbcUrl = Config.environment("jdbc.url") orElse properties.map(_.getProperty("jdbc.url"))
  def jdbcUser = Config.environment("jdbc.user") orElse properties.map(_.getProperty("jdbc.user"))
  def jdbcPw = Config.environment("jdbc.pw") orElse properties.map(_.getProperty("jdbc.pw"))  
  // For a better example, configure a connection pool
  def connection = Applicative build jdbcUrl and jdbcUser and jdbcPw apply java.sql.DriverManager.getConnection
  // Building subsystems using components.
  def dataStore = connection map (new DataStore(_))
  def workerPool = Config(new WorkerPool)
  def system = Applicative build dataStore and workerPool apply (new Application(_,_))
  def main(args: Array[String]): Unit = {
    system.get.getOrElse(error("Error reading configuration for main system")).doStuff
  }
}