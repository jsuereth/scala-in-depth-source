package scalax.resource

import scalax.functional.{Applicative, Functor, Monad, Implicits}
import Implicits._
import java.io._

object Example {
  
  type LazyTraversable[T] = collection.TraversableView[T, Traversable[T]]
  
  def makeLineTraversable(input: BufferedReader) = new Traversable[String] {
    def foreach[U](f: String => U): Unit = {
      var line = input.readLine()
      while (line != null) {
        f(line)
        line = input.readLine()
      }
    }
  } view
  
  def getLines(file: File): ManagedResource[LazyTraversable[String]] = 
    for {
      input <- ManagedResource.readFile(file) 
      val reader = new InputStreamReader(input)
      val buffered = new BufferedReader(reader)
    } yield makeLineTraversable(buffered)    
  
  
    
  def lineLengthCount(inFile: File, outFile: File) =
    for {
      lines <- getLines(inFile)
      val counts = lines.map(_.length).toSeq.zipWithIndex
      output <- ManagedResource.writeFile(outFile)
      val writer = new OutputStreamWriter(output)
      val buffered = new BufferedWriter(writer)
    } yield buffered.write(counts.mkString("\n"))
    
  def main(args: Array[String]): Unit = {
      workflow(new File("test.in"), new File("test.out")).loan(_ => ())
    }
  
}