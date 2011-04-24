trait Logger {
  def log(category : String, msg : String) : Unit = {
       println(msg)
  }
}

class DataAccess(val logger : Logger = new Logger {}) {

   def query[A](in : String) : A = {
     logger.log("QUERY", in)     
     null.asInstanceOf[A]
   }
}


class DoubleDataAccess(logger : Logger = DataAccess.`init$default$1`) extends DataAccess(logger) {}

/**
scala> val x = new DataAccess
x: DataAccess = DataAccess@15837e8

scala> x.query("HAI")
HAI
java.lang.NullPointerException
	at RequestResult$.<init>(<console>:9)
	at RequestResult$.<clinit>(<console>)
	at RequestResult$scala_repl_result(<console>)
	at sun.reflect.NativeMethodAccessorImpl.invoke0(Native Method)
	at sun.reflect.NativeMethodAccessorImpl.invoke(NativeMethodAccessorImpl.java:57)
	at sun.reflect.DelegatingMethodAccessorImpl.invoke(DelegatingMethodAccessorImpl.java:43)
	at java.lang.reflect.Method.invoke(Method.java:616)
	at scala.tools.nsc.Interpreter$Request$$anonfun$loadAndRun$1$$anonfun$apply$13.apply(Interpreter.scala:827)
	at scala.tools.nsc.Interpreter$Request$$anonfun$loadAndRun$1$$anonfun$apply$13.apply(Interpreter.scala:827)
	at scala.util.control.Exception$Catch.apply(Exception.scala:79)
	at scala.tools.nsc.Interpreter$Request$$anonfun$loadAndRun$1.apply(Interpreter.scala:826)
	at scala.tools.nsc.Interpreter$Request$$anonfun$loadAndRun$1.apply(Interpreter.scala:826)
	at scala.util.control.Exception$Catch.apply(Exception.scala:79)
	at scala.tools.nsc.Interpreter$Request.loadAndRun(Interpreter.scala:825)
	at scala.tools.nsc.Interpreter.interpret(Interpreter.scala:467)
	at scala.tools.nsc.Interpreter.interpret(Interpreter.scala:457)
	at scala.tools.nsc.InterpreterLoop.interpretStartingWith(InterpreterLoop.scala:391)
	at scala.tools.nsc.InterpreterLoop.command(InterpreterLoop.scala:367)
	at scala.tools.nsc.InterpreterLoop.processLine$1(InterpreterLoop.scala:249)
	at scala.tools.nsc.InterpreterLoop.repl(InterpreterLoop.scala:267)
	at scala.tools.nsc.InterpreterLoop.main(InterpreterLoop.scala:439)
	at scala.tools.nsc.MainGenericRunner$.createLoop$1(MainGenericRunner.scala:118)
	at scala.tools.nsc.MainGenericRunner$.main(MainGenericRunner.scala:143)
	at scala.tools.nsc.MainGenericRunner.main(MainGenericRunner.scala)

scala> val x = new DataAccess(logger = new Logger { def log(c : String, q : String) = println(c) })
<console>:6: error: overriding method log in trait Logger of type (category: String,msg: String)Unit;
 method log needs `override' modifier
       val x = new DataAccess(logger = new Logger { def log(c : String, q : String) = println(c) })
                                                        ^

scala> val x = new DataAccess(logger = new Logger { override def log(c : String, q : String) = println(c) })
x: DataAccess = DataAccess@1fc6eed

scala> x.query[AnyRef]("HAI")
QUERY
res1: AnyRef = null
**/

