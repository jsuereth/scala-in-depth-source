package scalax.functional


/**
 * This class represents a Functor from Category theory.  It is able to convert elements in the category of all types into
 * the sub-category of types of T[_].
 */
trait Functor[T[_]] {
   /** This method maps the given value into the functor category.   It is akin to a constructor. 
    */
   def apply[A](x: A): T[A]
   /** This method takes a an unmapped morphism and translates it into the new domain.
    * @param x A mapped value.
    * @param f A morphism in the original domain
    * @return The new value in the final domain.
    */
   def map[A,B](x: T[A])(f: A=>B): T[B]
}


object Functor {
  // Defaults for standard lib.
  implicit object TraversableFunctor extends Functor[Traversable] {
    override def apply[A](x: A) = Traversable(x)
    override def map[A,B](x:Traversable[A])(f: A=>B) = x map f
  }
  implicit object OptionFunctor extends Functor[Option] {
    override def apply[A](x: A) = Some(x)
    override def map[A,B](x: Option[A])(f: A=>B) = x map f
  }
}

/**
 * This class stores the 'pimped' functions used on values F[A] if F is a functor.
 */
final class FunctorOps[F[_], A](val value: F[A], val functor: Functor[F]) {
  @inline final def map[B](f: A=>B): F[B] = functor.map(value)(f)
}

/**
 * This trait stores all the implicit views to bring convenient syntax to functors.
 */
trait FunctorImplicits {
  implicit def funcOperations[F[_], A](value : F[A])(implicit functor: Functor[F]): FunctorOps[F,A] =
    new FunctorOps[F,A](value, functor)
}

