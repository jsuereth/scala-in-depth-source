/** Defines a trait that marks a class as allowing external
 * viewers to be notified of changes.
 */
trait Observable {
  // Defines a type returned when registering a callback.
  type Handle <: {
    def remove() : Unit
  }
  /** registered callbacks for this observable */
  protected var callbacks = Map[Handle, this.type => Unit]()

  /** Registers a new observer on this class.
   * Observers are simple functions that take the class and
   * return nothing.   A handle is returned that allows
   * the observer to deregister from this observable.
   */
  def observe(callback : this.type => Unit) : Handle = {
    val handle = createHandle(callback)
    callbacks += (handle -> callback)
    handle
  }
  /** Removes an observer from this class. */
  def unobserve(handle : Handle) : Unit = {
    callbacks -= handle
  }
  /** This method is called by subclasses upon state change to
   * notify listeners of the change.
   */
  protected def notifyListeners() : Unit =
    for(callback <- callbacks.values) callback(this)

  /**
   * Subclasses override this to provide their own callback disambiguation scheme.
   */
  protected def createHandle(callback : this.type => Unit) : Handle
}

/** This trait provides a default implementation
 * for the handlers type. */
trait DefaultHandles extends Observable {
  /** A simple handle implementation */
  class HandleClass {
    def remove() {
      DefaultHandles.this.unobserve(this)
    }
  }
  type Handle = HandleClass
  /** Every callback is assigned a new handle. */
  protected def createHandle(callback : this.type => Unit) : Handle = new HandleClass
}


/** This defines a basic store or cache of a single value type.
 * This store is observable and will notify listeners when the stored
 * value is changed.
 */
class VariableStore[X](private var value : X) extends Observable with DefaultHandles {
  /** Retreive the stored value */
  def get : X = value
  /** Sets the stored value.  Will notify observers */
  def set(newValue : X) : Unit = {
    value = newValue
    notifyListeners()
  }
  // Overridden for pretty REPL usage.
  override def toString : String = "VariableStore(" + value + ")"
}

/** This trait defines a mechanism for managing handles from
 * observerables such that they can all be unregistered when
 * the current class is 'finished'
 */
trait Dependencies {
  // This type allows us to refer to any handle from any
  // observable.  Because handle is defined inside the Observable
  // we use an existential for the actual Observable.
  type Ref = x.Handle forSome { val x: Observable }
  /** The current registered observers. */
  private var handles = List[Ref]()
  /** Adds a new handle to manage */
  protected def addHandle(handle : Ref) : Unit = {
    handles :+= handle
  }
  /** Removes all observers using the registrered handles */
  protected def removeDependencies() {
    for(h <- handles) h.remove()
    handles = List()
  }
  /** This method mimics Observable.observe except that it registers
   * an observer *and* adds it to the dependency list.
   */
  protected def observe[T <: Observable](obj : T)(handler : T => Unit) : Ref = {
    val ref = obj.observe(handler)
    addHandle(ref)
    ref
  }
}


/*

scala> val x = new VariableStore(12)
x: VariableStore[Int] = VariableStore(12)

scala> val d = new Dependencies {}
d: java.lang.Object with Dependencies = $anon$1@153e6f83


Note: Scala 2.8.x has a bug that causes the below to fail.

scala> d.addHandle(x.observe(println))
<console>:8: error: type mismatch;
 found   : x.Handle
 required: d.Ref
       d.addHandle(x.observe(println))
                            ^
// The following does work.

scala> val t = x.observe(println)
t: x.Handle = DefaultHandles$HandleClass@662fe032

scala> d.addHandle(t)

scala> val t2 = x.observe(println)
t2: x.Handle = DefaultHandles$HandleClass@57530551

scala> d.addHandle(t2)

scala> x.set(1)
VariableStore(1)
VariableStore(1)

scala> d.removeDependencies()

scala> x.set(2)

*/
