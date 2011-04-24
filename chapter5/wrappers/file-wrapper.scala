package test

import scala.collection.Traversable

/** This class wraps a java.io.File with a more powerful scala API. */
class FileWrapper(val file : java.io.File) {
  /** This method is used to compose File paths. Although called /, it will
   * do the right thing in windows.
   */
  def /(next : String) = new FileWrapper(new java.io.File(file, next))

  /**
   * This method will return a traversable collection that will iterate over the lines in the file.
   */
  def lines : Traversable[String] = new Traversable[String] {
    override def foreach[U](f : String => U) : Unit = {
      import java.io._
      val reader = new BufferedReader(new FileReader(file))
      try {
        var line = reader.readLine()
        while(line != null) {
          f(line)
          line = reader.readLine()
        }
      } finally {
        reader.close();
      }
    }
  }
  override def toString = file.getCanonicalPath
}

/**
 * The companion object to FileWrapper that can convert to and from java.io.File and FileWrapper.
 */
object FileWrapper {
  implicit def unwrap(w : FileWrapper) = w.file
  implicit def wrap(f : java.io.File) = new FileWrapper(f)
}

/** This object tests the FileWrapper implicit views. */
object Test {
  // This is the only implicit that needs to be improted for two-way conversions between wrapped and
  // straight java.io.File.
  import FileWrapper.wrap
  /** This method takes a java.io.File and prints its lines.  We use it to demonstrate implicit views. */
  def withFile(f : java.io.File) : Unit = {
    for(line <- f.lines) println(line)
  }

  def test() {
    val currentDirectory = new java.io.File(".")
    // wrapping and unwrapping is happening implicitly here.
    withFile( currentDirectory / "tmp.txt" )
  }
}