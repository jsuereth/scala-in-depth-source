/** Encoding of natural numbers in the type system, similar to church numerals */
sealed trait Nat {
  // This type is a mechanism for expansion of types over natural numbers.
  // Up is used as an upper bound for the resulting types of Exapnd.
  // NonZero is the type constructor to call against the previous natural number and
  // IfZero is the type to use when at the 'base' natural number, _0
  type Expand[NonZero[N <: Nat] <: Up, IfZero <: Up, Up] <: Up
}

object Nat {
  // Encoding for our 'zero' in the type system.
  sealed trait _0 extends Nat {
    // Expand also take steh IfZero case.
    type Expand[NonZero[N <: Nat] <: Ret, IfZero <: Ret, Ret] = IfZero
  }
  // Encoding for non-zero numbers based on a successive number.
  // Prev is the previous number in the type system.
  sealed trait Succ[Prev <: Nat] extends Nat {
    type Expand[NonZero[N <: Nat] <: Ret, IfZero <: Ret, Ret] = NonZero[Prev]
  }
  // Encode numbers 1->22 into the type system.   Note: This only lets us use numbers _1 through _22.
  type _1 = Succ[_0]
  type _2 = Succ[_1]  
  type _3 = Succ[_2]
  type _4 = Succ[_3]
  type _5 = Succ[_4]
  type _6 = Succ[_5]
  type _7 = Succ[_6]
  type _8 = Succ[_7]
  type _9 = Succ[_8]
  type _10 = Succ[_9]
  type _11 = Succ[_10]
  type _12 = Succ[_11]
  type _13 = Succ[_12]
  type _14 = Succ[_13]
  type _15 = Succ[_14]
  type _16 = Succ[_15]
  type _17 = Succ[_16]
  type _18 = Succ[_17]
  type _19 = Succ[_18]
  type _20 = Succ[_19]
  type _21 = Succ[_20]
  type _22 = Succ[_21]
}

/** Defines a heterogenously typed list.   FullType captures the full type of the HList */
trait HListLike[FullType <: HList] extends HList {
  def viewAt[Idx <: Nat](implicit in : FullType => FullType#ViewAt[Idx]) = in(this.asInstanceOf[FullType])
  def ::[T](v : T) = HCons(v, this.asInstanceOf[FullType])
}

/**The base type for HLists.  We can capture any Hlist using FullType <: HList and using various recursive algorithms
 * to deconstruct the list in a strongly typed fashion.   This can be very handy when passing heterogenously typed
 * lists that share a common type-class.
 */
sealed trait HList {
  /**This defines the type for a View of the list at a given index.   These views are used to modify
   * the list and to access values based on index.
   */
  type ViewAt[N <: Nat] <: IndexedView
}

/**
 * This class represents a link-node in the linked heterogenous list.   The type H refers to the type at the head
 * of the list, and the tail list is captures in the type T.
 */
final case class HCons[H, T <: HList](head : H, tail : T) extends HListLike[HCons[H,T]] {
  /**
   * This defines the type view of the list at a given index N.  This is done by recursively building up an HListViewX
   * classes using the natural numbers encoded in the type system.
   */
  type ViewAt[N <: Nat] = N#Expand[
    ({ type Z[P <: Nat] = HListViewN[H, T#ViewAt[P]] })#Z,
    HListView0[H,T], 
    IndexedView]
  override def toString = head + " :: " + tail
}

/**
 * This represents an empty heterogenious list.
 */
class HNil extends HListLike[HNil] {
  type ViewAt[N <: Nat] = Nothing
  override def toString = "HNil"
}

/**
 * The companion object to HList contains convenience definitions.
 */
object HList {
  // This defines the type operator to construct HList types using ::.
  type ::[H, T <: HList] = HCons[H,T]
  // Alias for HCons so that HList's can be constructed using ::
  val :: = HCons
  // The singleton HNil object used to construct HLists.   This technique is chosen such that HNil refers to both
  // the empty-list type and the empty-list value depending on context.
  val HNil = new HNil
  // This method recursively builds an indexed view for a list using implicit resolution.
  def indexedView[List <: HList,
                  Idx <: Nat](list : List)(implicit in : List => List#ViewAt[Idx]) = in(list)
}

import HList._

/**The base trait for indexed views.  This gives the type system a bottom type that we can call
 * methods against if needed.
 */
sealed trait IndexedView {
 type Before <: HList
 type After <: HList
 type At
 def fold[R](f : (Before, At, After) => R) : R
 def get = fold( (_, value, _) => value)
}

/** Companion object that stores the implicit factories for IndexedView classes */
object IndexedView {
  /** The base case:  An indexedview of the head of the list */
  implicit def index0[H, T <: HList](list : H :: T) : HListView0[H,T] =
    new HListView0[H,T](list)
  /** The recursive case: An indexedView that captures a new head and delegates to a view of the tail. */
  implicit def indexN[H, T <: HList, Prev <: IndexedView](
     list : (H :: T))(implicit indexTail : T => Prev) : HListViewN[H,Prev] =
       new HListViewN[H, Prev](list.head, indexTail(list.tail))
}


/**
 * This represents the view of an HList from its head.  As such, both the head and tail types of the list are
 * captured and fold is defined against the head of the list.
 */
final class HListView0[H, T <: HList](val list : H :: T) extends IndexedView {
  type Before = HNil
  type After = T
  type At = H
  def fold[R](f : (Before, At, After) => R): R =
    f(HNil, list.head, list.tail)
}

/**
 * This represents the view of an HList where the current index is *not* the desired index for the view.
 * This class builds from an IndexedView against a tail of list.   It captures one element preceding the tail.
 * This mechanism can be used to indexed anywhere in a list, by creating a HListView0 at the desired index and
 * building out HListViewN's to capture the preceding values of the list.
 */
final class HListViewN[H, NextIdxView <: IndexedView](h : H, next : NextIdxView) extends IndexedView {
  type Before = H :: NextIdxView#Before
  type At = NextIdxView#At
  type After = NextIdxView#After
  def fold[R](f : (Before, At, After) => R) : R = 
    next.fold( (before, at, after) =>
      f(HCons(h, before), at, after) )
}


