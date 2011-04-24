
trait Observable {
  type Handle

  protected var callbacks = Map[Handle, this.type => Unit]()

  def observe(callback : this.type => Unit) : Handle = {
    val handle = createHandle(callback)
    callbacks += (handle -> callback)
    handle
  }

  def unobserve(handle : Handle) : Unit = {
    callbacks -= handle
  }

  protected def notifyListeners() : Unit =
    for(callback <- callbacks.values) callback(this)

  /**
   * Subclasses override this to provide their own callback disambiguation scheme.
   */
  protected def createHandle(callback : this.type => Unit) : Handle
}

trait DefaultHandles extends Observable {
  type Handle = (this.type => Unit)
  protected def createHandle(callback : this.type => Unit) : Handle = callback
}



class VariableStore[X](private var value : X) extends Observable with DefaultHandles {
  def get : X = value
  def set(newValue : X) : Unit = {
    value = newValue
    notifyListeners()
  }

  override def toString : String = "VariableStore(" + value + ")"
}

/**
Welcome to Scala version 2.8.0.RC3 (OpenJDK 64-Bit Server VM, Java 1.6.0_18).
Type in expressions to have them evaluated.
Type :help for more information.

scala> val x = new VariableStore(5)
x: VariableStore[Int] = VariableStore(5)

scala> val handle = x.observe(println)
handle: (x.type) => Unit = <function1>

scala> x.set(2)
VariableStore(2)

scala> x.unobserve(handle)

scala> x.set(4)

 */


/**
 * Type difference != runtime difference.

scala> val x = new VariableStore(5)   
x: VariableStore[Int] = VariableStore(5)

scala> val y = new VariableStore(2)
y: VariableStore[Int] = VariableStore(2)                              ^

scala> val callback = println(_ : Any)
callback: (Any) => Unit = <function1>

scala> val handle1 = x.observe(callback)
handle1: (x.type) => Unit = <function1>

scala> val handle2 = y.observe(callback)
handle2: (y.type) => Unit = <function1>

scala> y.set(3)
VariableStore(3)

scala> x.set(5)
VariableStore(5)

scala> y.unobserve(handle1)
<console>:10: error: type mismatch;
 found   : (x.type) => Unit
 required: (y.type) => Unit
       y.unobserve(handle1)
                   ^

scala> handle

handle1   handle2

scala> handle1 == handle2
res3: Boolean = true

 */
