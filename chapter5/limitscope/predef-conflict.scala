object Time {
  case class TimeRange(start : Long, end : Long)
  implicit def longWrapper(start : Long) = new {
    def to(end : Long) = TimeRange(start, end)
  }
}

object Test {
  println(1L to 10L)
  import Time._
  println(1L to 10L)
  def x() = {
    import scala.Predef.longWrapper
    println(1L to 10L)
    def y() = {
      import Time.longWrapper
      println(1L to 10L)
    }
    y()
  }
  x()
}