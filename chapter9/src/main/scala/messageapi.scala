package scattergather

import akka.actor.ActorRef

/**
 * Represents a Search Query that is sent through the actors system.
 */
case class SearchQuery(query : String, maxDocs : Int, gatherer : ActorRef)

/**
 * Represents a partial or full response of query results.
 */
case class QueryResponse(results : Seq[(Double, String)])