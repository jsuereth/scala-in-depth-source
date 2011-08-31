package scalax.config

import java.sql.DriverManager
import util.control.Exception.catching
import com.github.jsuereth.applicative

import scalax.functional.{Functor,Monad,Applicative}
import scalax.functional.Implicits._

/**Represents something that is read from the 'environment' before usage.
 * The 'environment' in question could be one of several locations.
 *
 */
trait Config[+A] { self =>
  def get : Option[A]
  def flattenOpt[B](implicit ev: Option[A] <:< Option[Option[B]]): Config[B] = new OptionFlattenConfig(this)
  def orElse[B >: A](b : Config[B]) : Config[B] = new Combiner(this, b)
}

// a 'file' environment monad that attempts to minimize reads to the file based
// on the file timestamp.
final class FileReaderConfig[A](file : java.io.File, reader : java.io.File => A) extends Config[A] {
  // TODO - Thread safety...
  private var cache  = Option.empty[A]
  private var timestamp = Option.empty[Long]
  def get : Option[A] =
    (for {
      time <- timestamp
      if time == file.lastModified
      value <- cache
    } yield value) orElse {
      cache = (catching[A](classOf[java.io.IOException]) opt reader(file))
      timestamp = Some(file.lastModified)
      cache
    }
}

object Config {
  /* Functor implementation. */
  implicit object ConfigFunctor extends Functor[Config] {
    override def apply[A](x: A) = Config(x)
    override def map[A,B](x: Config[A])(f: A=>B) = new MappedConfig(x, f)
  }
  implicit object ConfigApplicative extends Applicative[Config] {
    override def lift2[A,B](f: Config[A => B])(ma: Config[A]): Config[B] = 
      new FlatMappedConfig(ma, (z: A) => f.map(_(z)))
  }
    /* Monad implementation */
  implicit object ConfigMonad extends Monad[Config] {
    override def flatten[A](m: Config[Config[A]]): Config[A] =
      new FlatMappedConfig(m, identity[Config[A]])
    override def flatMap[A,B](m: Config[A])(f: A=>Config[B])(implicit functor : Functor[Config]): Config[B] =
      new FlatMappedConfig(m, f)
  }
  def environment(name : String) : Config[String] =
    Config(if (System.getenv.containsKey(name))
      Some(System.getenv.get(name))
    else None).flattenOpt
  def systemproperty(name : String) : Config[String] =
    Config(Option(System.getProperty(name, null))).flattenOpt
  def propertyfile(file : java.io.File) : Config[java.util.Properties] =
    new FileReaderConfig(file, { file =>
      val tmp = new java.util.Properties()
      Console.println("Loading file: " + file)
      tmp.load(new java.io.FileReader(file))
      tmp
    })
  def apply[A](a : => A): Config[A] = new Config[A] { def get : Option[A] = Some(a) }
}
// Implementation detail of the Config monad when it's mapped.   We need
// to collect and defer the operation.
final class MappedConfig[O,A](val orig : Config[O], f : O => A) extends Config[A] {
  override def get = orig.get map f
}
// Implementation detail for flatmapped configuration.
final class FlatMappedConfig[O,A](val orig : Config[O], f : O => Config[A]) extends Config[A] {
  override def get = orig.get flatMap (a => f(a).get)
}

// This combines two config objects
final class Combiner[A](config : Config[A], config2 : Config[A]) extends Config[A] {
  def get: Option[A] = config.get orElse config2.get
}

final class OptionFlattenConfig[A,B](config: Config[A])(implicit ev: Option[A]<:<Option[Option[B]]) extends Config[B] {
  override def get: Option[B] = config.get.flatten
}

// This example shows how to use the manual "orElse" method.
// Later, we'll show how this orElse can be abstracted to use the Monoid concept.
// TODO - Move orElse into here like the previous object.
object MultiConfigLocationSimple {
  // A Config which reads the myapp.properties file when accessed.
  val configFile = Config.propertyfile(new java.io.File("myapp.properties"))
  // A function that takes a Config for a properties object and turns it into
  // a Config for a single property on the Config.
  def propertyOnFile(name : String, config : Config[java.util.Properties]) =
    config map { value =>
        if (value.containsKey(name))
          Some(value.getProperty(name))
        else None
    } flattenOpt
  val url =
    propertyOnFile("url", configFile) orElse
    Config.environment("jdbc_url")
}