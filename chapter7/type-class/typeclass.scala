import java.io._

/**
 * Defines an interface for things that are like files for our synchronization code.
 */
trait FileLike {
  def name : String
  def exists : Boolean
  def isDirectory : Boolean
  def parent : FileLike
  def children : Seq[FileLike]
  // Creates new child of given name under this directory (throws if this is not a directory)
  def child(name : String) : FileLike
  def mkdirs() : Unit

  def content : InputStream
  // This will write a new file if it doesn't exist
  def writeContent(otherContent : InputStream) : Unit
}

object SynchUtil {

  def synchronize[F <: FileLike,
                  T <: FileLike](
                  from : F,
                  to : T) : Unit = {

    def synchronizeFile(file1 : F,
                        file2 : T) : Unit = {
      file2.writeContent(file1.content)
    }

    def synchronizeDirectory(dir1 : F,
                             dir2 : T) : Unit = {
      def findFile(file : FileLike,
                   directory : FileLike) : Option[FileLike] =
        (for { file2 <- directory.children
          if file.name == file2.name
        } yield file2).headOption

      for(file1 <- dir1.children) {
        val file2 = findFile(file1, dir2).
                getOrElse(dir2.child(file1.name))
        if(file1.isDirectory) {
          file2.mkdirs()
        }
        synchronize[F,T](file1, file2)
      }
    }

    if(from.isDirectory) {
      synchronizeDirectory(from,to)
    } else {
      synchronizeFile(from,to)
    }
  }
}
