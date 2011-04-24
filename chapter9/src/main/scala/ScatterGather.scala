package scattergather

import collection.immutable.HashMap
import akka.actor.{ReceiveTimeout, ActorRef, Actor}

/** Defines a SearchNode that contains a fragment of the search index */
class SearchNode(id : Int) extends Actor {
  // TODO - index init
  lazy val index : HashMap[String, Seq[(Double, String)]] = {
    def makeIndex(docs : String*) = {
      var tmp = HashMap[String, Seq[(Double, String)]]()
      for( doc <- docs; (key,value) <- doc.split("\\s+").groupBy(identity)) {
        val list = tmp.get(key) getOrElse Seq()
        tmp += ((key, ((value.length.toDouble, doc)) +: list))
      }
      tmp
    }
    id match {
      case 1 => makeIndex("Some example data for you")
      case 2 => makeIndex("Some more example data for you to use")
      case 3 => makeIndex("To be or not to be, that is the question")
      case 4 => makeIndex("OMG it's a cat")
      case 5 => makeIndex("This is an example.  It's a great one")
      case 6 => makeIndex("HAI there", "HAI IZ HUNGRY")
      case 7 => makeIndex("Hello, World")
      case 8 => makeIndex("Hello, and welcome to the search node 8")
      case 9 => makeIndex("The lazy brown fox jumped over the")
      case 10 => makeIndex("Winning is the best because it's winning.")
    }
  }
  def receive = {
    case SearchQuery(query, maxDocs, handler) =>
      val result = for {
        results <- index.get(query).toList
        resultList <- results
      } yield resultList
      handler ! QueryResponse(result.take(maxDocs))
  }
}

/** The head node for the search tree.  Note:  The tree can be multiple levels deep, with head nodes
 * pointing to other head nodes.
 */
class HeadNode extends Actor {
  // TODO - Init search nodes
  def nodes : Seq[ActorRef] = Seq()
  def receive = {
     case SearchQuery(q, max, responder) =>
        val gatherer = Actor.actorOf(new GathererNode {
          val maxDocs = max
          val maxResponses = nodes.size
          val query = q
          val client = responder
        })
        gatherer.start
        for (node <- nodes) {
          node ! SearchQuery(q, max, gatherer)
        }
  }
}
/** An actor which receives distributed results and aggregates/responds to the original query. */
trait GathererNode extends Actor {
  val maxDocs : Int
  val query : String
  val maxResponses : Int
  val client : ActorRef
  self.receiveTimeout = Some(1000L)
  /** Stores the current set of results */
  var results = Seq[(Double, String)]()
  var responseCount = 0
  /** Combines the current reuslts with the next set of search results. */
  private def combineResults(current : Seq[(Double, String)], next : Seq[(Double, String)]) =
    (current ++ next).view.sortBy(_._1).take(maxDocs).force

  def receive = {
    case QueryResponse(next) =>
      results = combineResults(results, next)
      responseCount += 1
      if(responseCount == maxResponses) {
        client ! QueryResponse(results)
        self.stop()
      }
      ()
    case ReceiveTimeout =>
      client ! QueryResponse(Seq())
      self.stop()
  }
}