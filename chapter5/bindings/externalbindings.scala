package test;

// The x in this file is used to demenstrate an externally bound x within the testbindings.scala file.
// These two files should be compiled together.
object x {
  override def toString = "Externally bound x object in package test"
}
