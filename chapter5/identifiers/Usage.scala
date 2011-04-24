import test.{Foo=>Bar}

object Test {
  def main(args: Array[String]) : Unit = {
    val x = new Bar
    println( "Created new: " + x)
  }
}
