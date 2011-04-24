class Test {
  def first[A : ClassManifest](x : Array[A]) = Array(x(0))
}
