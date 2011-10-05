package scattergather

import akka.actor.{Supervisor, Actor, ActorRef}
import akka.config.Supervision._
import akka.dispatch.Dispatchers
import java.util.concurrent.ThreadPoolExecutor.CallerRunsPolicy

object AdaptiveSearchTreeMain {
  def submitInitialDocuments(searchNode: ActorRef) =
    Seq("Some example data for you",
        "Some more example data for you to use",
        "To be or not to be, that is the question",
        "OMG it's a cat",
        "This is an example.  It's a great one",
        "HAI there", 
        "HAI IZ HUNGRY",
        "Hello, World",
        "Hello, and welcome to the search node 8",
        "The lazy brown fox jumped over the",
        "Winning is the best because it's winning."
    ) foreach (doc =>  searchNode ! SearchableDocument(doc))
  def makeTree = {
    val supervisor = Supervisor(SupervisorConfig(AllForOneStrategy(List(classOf[Exception]), 3, 1000), Nil))
    val searchnodedispatcher = Dispatchers.newExecutorBasedEventDrivenDispatcher("adaptive search tree")
        .withNewThreadPoolWithLinkedBlockingQueueWithCapacity(100)
        .setCorePoolSize(10)
        .setMaxPoolSize(128)
        .setKeepAliveTimeInMillis(60000)
        .setRejectionPolicy(new CallerRunsPolicy)
        .build
    val searchTree = Actor.actorOf(new AdaptiveSearchNode {
      self.dispatcher = searchnodedispatcher
    })
    supervisor link searchTree
    searchTree.start()
    submitInitialDocuments(searchTree)
    searchTree
  }
}

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
