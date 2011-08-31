package scalax.functional

/** This class represents the ability to 'flatten' a Functor morphism.  That is,
 * If we have a functor F[_] applied twice, e.g. List[List[Int]], then this typeclass can reduce it to a single functor application,
 * i.e. a List[Int].
 */
trait Monad[M[_]] {
  /**
   * This method takes two functor applications and reduces them to a single application.
   * 
   * Essentially this method can take containers nested into containers and remove the outter container.
   */
  def flatten[A](m: M[M[A]]): M[A]
  /**
   * This method is provided because it is often more efficient to directly implement a flatMap method rather than use
   * map and flatten.
   */
  def flatMap[A,B](m: M[A])(f: A=>M[B])(implicit functor : Functor[M]): M[B] = flatten(functor.map(m)(f))
}

object Monad {
  implicit object TraversableFunctor extends Monad[Traversable] {
    override def flatten[A](m: Traversable[Traversable[A]]): Traversable[A] = m.flatten
    override def flatMap[A,B](m: Traversable[A])(f: A=>Traversable[B])(implicit functor : Functor[Traversable]): Traversable[B] =
      m flatMap f
  }
  implicit object OptionFunctor extends Monad[Option] {
    override def flatten[A](m: Option[Option[A]]): Option[A] = m flatMap identity
    override def flatMap[A,B](m: Option[A])(f: A=>Option[B])(implicit functor : Functor[Option]): Option[B] =
      m.flatMap(f)
  }
}

final class MonadOps[M[_], A](val m : M[A], val monad: Monad[M]) {
  @inline final def flatMap[B](f: A=>M[B])(implicit functor: Functor[M]): M[B] = monad.flatMap(m)(f)
  @inline final def flatten[B](implicit ev : M[A] <:< M[M[B]]): M[B] = monad.flatten(m)
}

trait MonadImplicits {
  implicit def monadOps[M[_], A](value: M[A])(implicit monad: Monad[M]) : MonadOps[M, A] =
    new MonadOps[M,A](value, monad)
}