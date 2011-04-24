package sperformance

import collection.immutable.{HashMap=>ImmutableHashMap}
import collection.mutable.{HashMap=>MutableHashMap}
import java.util.concurrent.{ExecutorService,Executors}
import annotation.tailrec

// Abstract type for memoizing function values.
trait Service[Key,Value] {
  def lookUp(k : Key) : Option[Value]
  def insert(k : Key, v : Value) : Unit
}


class ImmutableService[Key, Value] extends Service[Key, Value] {
  var currentIndex = new ImmutableHashMap[Key,Value]
  def lookUp(k : Key) : Option[Value] = currentIndex.get(k)
  def insert(k : Key, v: Value) : Unit = synchronized {
    currentIndex = currentIndex + ((k, v))
  }
}


class MutableService[Key, Value] extends Service[Key, Value] {
  val currentIndex = new MutableHashMap[Key, Value]
  def lookUp(k : Key) : Option[Value] = synchronized(currentIndex.get(k))
  def insert(k : Key, v : Value) : Unit = synchronized {
    currentIndex.put(k,v)
  }
}


object TestLibs {
  var executor = Executors.newFixedThreadPool(2)

  implicit def functionToCallable[A](f : () => A) = new Runnable {
    override def run() {
      f()
    }
  }

  def runTest(r : Stream[Runnable]) {

  }

  class ListHolder {
    var list = List[Boolean]()

    def addValue(x : Boolean) : Unit = synchronized {
      list = x :: list
    }
  }

  def makeTest(n : Int, service : Service[Int, Int]) : (List[Runnable], ListHolder) = {
    var results = new ListHolder()
    def makeInsertRunnable(idx : Int) : Runnable = new Runnable {
      override def run() {
        service.insert(idx,idx)
      }
    }
    def makeReadRunnable(idx : Int) : Runnable = new Runnable {
      override def run() {
        val result = service.lookUp(idx)
        //Thread.sleep(10L)
        results.addValue(result.isDefined)
      }
    }
    def makeStream(generateIdx : Int, readIdx : Int, max : Int) : Stream[Runnable] = {
      (generateIdx, readIdx, math.random) match {
        case (i, j, k) if j > max =>
          Stream()
        case (i, j, k) if i > max =>
          Stream.cons(makeReadRunnable(j), makeStream(i, j+1, max))
        case (i, j, k) if i == j =>
          Stream.cons(makeInsertRunnable(i), makeStream(i+1, j, max))
        case (i, j, k) if k > 0.5 =>
          Stream.cons(makeInsertRunnable(i), makeStream(i+1, j, max))
        case (i, j, k) =>
          Stream.cons(makeReadRunnable(j), makeStream(i, j+1, max))
      }
    }
    (makeStream(1,1,n).toList, results)
  }
  

}

import _root_.java.util.concurrent._

object MapConcurrencyTest extends sperformance.dsl.PerformanceDSLTest {

  performance of "ImmutableService" in {
    measure method "index-lookup" in {
      val x = Executors.newFixedThreadPool(2)
      withSize upTo 50 withSetup {
        size =>
          TestLibs.makeTest(size, new ImmutableService)
      } run {
        case (runnables, results) =>
          // Ensure we wait until all runnables are done
          runnables.map(x.submit(_, true)).foreach(_.get)
      }
    }
  }
  performance of "MutableService" in {
    measure method "index-lookup" in {
      val x = Executors.newFixedThreadPool(2)
      withSize upTo 50 withSetup {
        size =>
          TestLibs.makeTest(size, new MutableService)
      } run {
        case (runnables, results) =>
          // Ensure we wait until all runnables are done
          runnables.map(x.submit(_, true)).foreach(_.get)
      }
    }
  }
}