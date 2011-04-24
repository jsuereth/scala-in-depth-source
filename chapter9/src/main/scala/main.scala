package scattergather

import akka.actor.{Supervisor, Actor}
import akka.config.Supervision._
import akka.dispatch.Dispatchers
import java.util.concurrent.ThreadPoolExecutor.CallerRunsPolicy

object SearchTreeMain {
  def create(name: String) = {
    val supervisor = Supervisor(SupervisorConfig(AllForOneStrategy(List(classOf[Exception]), 3, 1000), Nil))
    val searchnodedispatcher = Dispatchers.newExecutorBasedEventDrivenDispatcher("search tree [" + name + "]")
      .withNewThreadPoolWithLinkedBlockingQueueWithCapacity(100)
      .setCorePoolSize(10)
      .setMaxPoolSize(128)
      .setKeepAliveTimeInMillis(60000)
      .setRejectionPolicy(new CallerRunsPolicy)
      .build
    val leafNodes = (1 to 10).map(i => Actor.actorOf(new SearchNode(i) {
      self.dispatcher = searchnodedispatcher
    }))
    leafNodes foreach supervisor.link
    leafNodes foreach (_.start())
    val headNode = Actor.actorOf(new HeadNode {
      override val nodes = leafNodes
      self.dispatcher = searchnodedispatcher
    })
    supervisor link headNode
    headNode.start()
    headNode
  }

  def makeResponder = {
    val tmp = Actor.actorOf(new Actor {
      def receive = {
        case x => println(x)
      }
    })
    tmp.start()
    tmp
  }

}