package scalax.resource

import scalax.functional.{Applicative,Functor,Monad}


trait ManagedResource[T] {
  // Note: This operation could fail.
  def loan[U](f: T => U): U
}

object ManagedResource {
  def readFile(file: java.io.File) = new ManagedResource[java.io.InputStream] {
    def loan[U](f: java.io.InputStream => U): U = {
      val stream = new java.io.BufferedInputStream(new java.io.FileInputStream(file))
      try {
        f(stream)
      } finally {
        stream.close()
      }
    }
  }
  
  def writeFile(file: java.io.File) = new ManagedResource[java.io.OutputStream] {
    def loan[U](f: java.io.OutputStream => U): U = {
      val stream = new java.io.BufferedOutputStream(new java.io.FileOutputStream(file))
      try {
        f(stream)
      } finally {
        stream.close()
      }
    }
  }
  
  def make[T <: { def close(): Unit }](t: => T): ManagedResource[T] = 
    new ManagedResource[T] {
      def loan[U](f: T => U): U = {
        val resource = t
        try {
          f(resource)
        } finally {
          resource.close()
        }
      }
      override def toString = "ManagedResource(...)"
    }
  
  implicit object MRFunctor extends Functor[ManagedResource] {
    override def apply[A](a: A) = new ManagedResource[A] {
      override def loan[U](f: A => U) = f(a)
      override def toString = "ManagedResource("+a+")"
    }
    override def map[A,B](ma: ManagedResource[A])(mapping: A => B) = new ManagedResource[B] {
      override def loan[U](f: B => U) = ma.loan(mapping andThen f)
      override def toString = "ManagedResource.map("+ma+")("+mapping+")"
    }
  }
  implicit object MRMonad extends Monad[ManagedResource] {
    override def flatten[A](mma: ManagedResource[ManagedResource[A]]): ManagedResource[A] =
      new ManagedResource[A] {
        override def loan[U](f: A => U): U = mma.loan(ma => ma.loan(f))
        override def toString = "ManagedResource.flatten("+mma+")"
      }
  }
  implicit object MRApplicative extends Applicative[ManagedResource] {
    override def lift2[A,B](func: ManagedResource[A=>B])(ma: ManagedResource[A]): ManagedResource[B] =
      new ManagedResource[B] {
        override def loan[U](f: B => U): U = func.loan(n => ma.loan(n andThen f))
        override def toString = "ManagedResource.lift2("+func+")("+ma+")"
      }
  }
}