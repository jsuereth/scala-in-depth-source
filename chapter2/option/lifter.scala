

object Lifter {

  def foo(a : String, b : String) = a + b

  def lift3[A,B,C,D](f : Function3[A,B,C,D]) : Function3[Option[A], Option[B], Option[C], Option[D]] = {
    (oa : Option[A], ob : Option[B], oc : Option[C]) =>
       for(a <- oa; b <- ob; c <- oc) yield f(a,b,c)
  }

}