package object foo {
  implicit def foo = new Foo
}

package foo {
  class Foo {
    override def toString = "FOO!"
  }
}
