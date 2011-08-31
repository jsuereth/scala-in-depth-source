package scalax.functional

/**
 * This trait represents...
 */
trait Applicative[F[_]] {
  /**
   * This function is responsible for taking a mapped morphism (i.e. F[A=>B] where A=>B is the 'morphism' or function) and
   * converting into a function against the mapped domain, i.e. F[A] => F[B].
   * 
   * Note: This is similar to Haskell's <*> function.
   */
  def lift2[A,B](f: F[A=>B])(ma: F[A]): F[B]
}

object Applicative {
  implicit def makeBuilder[F[_] : Functor : Applicative,A](ma: F[A]) = new ApplicativeBuilder[F,A](ma)
  def apply[F[_]: Functor: Applicative, A](m: F[A]) = makeBuilder(m)
  def build[F[_]: Functor: Applicative, A](m: F[A]) = makeBuilder(m)
  def foo(x: String*) = null
  def lift[F[_]: Functor: Applicative] = new {
    val func = implicitly[Functor[F]] 
    val app = implicitly[Applicative[F]]
    def apply3[A,B,C,D](f: (A,B,C) => D): (F[A], F[B], F[C]) => F[D] = {
       (fa, fb, fc) =>
          val tmp: F[B => C => D] = func.map(fa)(f.curried)
          val tmp2: F[C => D] = app.lift2(tmp)(fb)
          app.lift2(tmp2)(fc)
    }
  }
  
  implicit object OptionApplicative extends Applicative[Option] {
    def lift2[A,B](f: Option[A=>B])(ma: Option[A]): Option[B] = for {
      func <- f
      a <- ma
    } yield func(a)
  }
  implicit object TraversableApplicative extends Applicative[Traversable] {
    def lift2[A,B](f: Traversable[A=>B])(ma: Traversable[A]): Traversable[B] = for {
      func <- f
      a <- ma
    } yield func(a)
  }
}

class ApplicativeBuilder[F[_],A](ma: F[A])(implicit functor: Functor[F], ap: Applicative[F]) {
  import Implicits._  
  def and[B](mb: F[B]) = new ApplicativeBuilder2(mb)
  def apply[B](f: A => B): F[B] = ma.map(f)
  class ApplicativeBuilder2[B](mb: F[B]) {
    def apply[C](f: (A, B) => C): F[C] = ap.lift2((ma.map(f.curried)))(mb)
    def and[C](mc: F[C]) = new AppplicativeBuilder3[C](mc)
    class AppplicativeBuilder3[C](mc: F[C]) {
      def apply[D](f: (A,B,C) => D): F[D] = ap.lift2(ap.lift2((ma.map(f.curried)))(mb))(mc)
      def and[D](md: F[D]) = new ApplicativeBuilder4[D](md)
      class ApplicativeBuilder4[D](md: F[D]) {
        def apply[E](f: (A,B,C,D) => E): F[E] = ap.lift2(ap.lift2(ap.lift2((ma.map(f.curried)))(mb))(mc))(md)
        def and[E](me: F[E]) = new ApplicativeBuilder5[E](me)
        class ApplicativeBuilder5[E](me: F[E]) {
          def apply[R](f: (A,B,C,D,E) => R): F[R] = ap.lift2(ap.lift2(ap.lift2(ap.lift2((ma.map(f.curried)))(mb))(mc))(md))(me)
        }
      }
    }
  }
}
 

trait ApplicativeMagic[F[_]] {
  def apply[C](f: ApplicativeMagicFunctionHolder[FunctionArg => C]): F[C]
  type FunctionArg
}
class ApplicativeMagicFunctionHolder[F](val f: F)
object ApplicativeMagicFunctionHolder {
  implicit def fix2[A,B,C](f: (A,B) => C): ApplicativeMagicFunctionHolder[Tuple2[A,B] => C] =
    new ApplicativeMagicFunctionHolder(f.tupled)
  implicit def fix3[A,B,C,D](f: (A,B,C) => D): ApplicativeMagicFunctionHolder[Tuple2[Tuple2[A,B],C] => D] =
    new ApplicativeMagicFunctionHolder({ case ((a,b),c) => f(a,b,c) })
  implicit def fix4[A,B,C,D,E](f: (A,B,C,D) => E): ApplicativeMagicFunctionHolder[Tuple2[Tuple2[Tuple2[A,B],C],D] => E] = 
    new ApplicativeMagicFunctionHolder({ case (((a,b),c),d) => f(a,b,c,d) })
  implicit def fix5[A,B,C,D,E,F](f: (A,B,C,D,E) => F): ApplicativeMagicFunctionHolder[Tuple2[Tuple2[Tuple2[Tuple2[A,B],C],D],E] => F] = 
    new ApplicativeMagicFunctionHolder({ case ((((a,b),c),d),e) => f(a,b,c,d,e) })   
  def apply[A,C](f: A => C) = new ApplicativeMagicFunctionHolder(f)
}
object ApplicativeMagic {
  def apply[F[_], A](ma: F[A])(implicit functor: Functor[F], app: Applicative[F]) = new ApplicativeMagic1[F,A](ma)(functor, app)
}
class ApplicativeMagic1[F[_],A](ma: F[A])(implicit functor: Functor[F], app: Applicative[F]) extends ApplicativeMagic[F] {
  import Implicits._
  type FunctionArg = A
  override def apply[C](f: ApplicativeMagicFunctionHolder[FunctionArg => C]): F[C] = ma map f.f
  def <*>[B](mb: F[B]) = new ApplicativeMagicN[F,B,this.type](mb, this)
}
class ApplicativeMagicN[F[_], B, Magic <: ApplicativeMagic[F]](mn: F[B], prev: Magic)(implicit functor: Functor[F], app: Applicative[F]) extends ApplicativeMagic[F] {
  import Implicits._
  type FunctionArg = (Magic#FunctionArg, B)
  override def apply[C](f: ApplicativeMagicFunctionHolder[FunctionArg => C]): F[C] = {
    val f2: Magic#FunctionArg => B => C = args => b => f.f((args,b))
    app.lift2(prev[B=>C](ApplicativeMagicFunctionHolder(f2)))(mn)
  }
  def <*>[C](mc: F[C]) = new ApplicativeMagicN[F,C,this.type](mc, this)
}

/*
object AppMagic

trait AppMagic[F[_]] {
 type ReturnType[B]
 type ArgType
 type CurriedFuncType[B] = ArgType => ReturnType[B]
 def apply[B](f: CurriedFuncType[B]): F[B]
 def fmap[B](f: F[CurriedFuncType[B]]): F[B]
}

class AppMagicN[A, F[_] : Functor : Applicative, T <: AppMagic[F]](prev: T, mb: F[A]) extends AppMagic[F] {
  type ReturnType[B] = T#CurriedFuncType[B]
  type ArgType = A
  def apply[B](f: CurriedFuncType[B]): F[B] = fmap(implicitly[Functor[F]].apply(f))
  def fmap[B](f: F[CurriedFuncType[B]]): F[B] = prev.fmap(implicitly[Applicative[F]].lift2(f)(mb))
}

class AppMagic1[A, F[_]: Functor : Applicative](val ma: F[A]) extends AppMagic[F] {
  type ReturnType[B] = B
  type ArgType = A
  import Implicits._
  
  def <*>[B](mb: F[B]) = {
    val tmp = new AppMagic1[B,F](mb)
    new AppMagicN[A, F, tmp.type](tmp, ma)
  }
  def apply[B](f: CurriedFuncType[B]): F[B] = ma map f
  def fmap[B](f: F[CurriedFuncType[B]]): F[B] = implicitly[Applicative[F]].lift2(f)(ma)
}

*/