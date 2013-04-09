package org.ffplanner

import scala.language.implicitConversions
import org.ffplanner.ConflictGraph.Node

/** A graph whose nodes contain showings (of a movie). An edge between two showing nodes indicates that the two are in
  * conflict.
  *
  * @author Bogdan Dumitriu
  */
class ConflictGraph(val festivalProgramme: FestivalProgramme) {

  private val graph: collection.mutable.Map[Node, collection.mutable.Set[Node]] =
    collection.mutable.Map.empty.withDefaultValue(collection.mutable.Set.empty)

  private implicit def createNode(showingId: Long): Node = new Node(showingId)

  /** Initializes the graph such that all scheduleable showings of the movies in `scheduleConstraints` are added. A
    * showing is not scheduleable if it conflicts with the showings in `scheduleConstraints`.
    */
  def initializeWith(scheduleConstraints: ScheduleConstraints) {
    def addMovieShowingsToGraph(movieId: Long) {
      festivalProgramme.showingsOf(movieId).foreach(addShowingToGraph(_))
    }

    def addShowingToGraph(showing: Showing) {
      val conflicts: Set[Long] = festivalProgramme.getConflictsOf(
        showing.id, scheduleConstraints.movieConstraintIds, scheduleConstraints.showingConstraintIds)
      if (conflicts.intersect(scheduleConstraints.showingConstraintIds).isEmpty) {
        graph(showing.id) = collection.mutable.Set(conflicts.toList: _*).map(createNode)
      }
    }

    graph.clear()
    scheduleConstraints.movieConstraintIds.foreach(addMovieShowingsToGraph(_))
  }

  /** Registers the fact that a showing has been scheduled. The showing, all its direct neighbours and all the other
    * showings of the same movie as well as their direct neighbours will be deleted.
    */
  def updateWith(scheduledShowing: Showing) {
    def deleteNodeAndNeighbours(showingId: Long) {
      graph.remove(showingId).foreach(neighbours => neighbours.foreach { deleteNode(_) })
    }

    festivalProgramme.showingsOf(scheduledShowing.movie).foreach { showing => deleteNodeAndNeighbours(showing.id) }
  }

  /** Registers the fact that a showing has been scheduled. The showing, all its direct neighbours and all the other
    * showings of the same movie as well as their direct neighbours will be deleted.
    */
  def updateWith(scheduledShowingId: Long) {
    updateWith(festivalProgramme.getShowing(scheduledShowingId))
  }

  def hasShowing(showingId: Long): Boolean = graph.contains(showingId)

  def neighboursOf(showingId: Long): Set[Node] = graph(showingId).toSet

  def getIsolatedNodes: Set[Node] = getNodesWithNeighbourCount(0)

  def getNodesWithNeighbourCount(nrNeighbours: Int): Set[Node] = {
    graph.keySet.toSet filter { graph(_).size == nrNeighbours }
  }

  /** Deletes the `node` and all the edges leading to it form the graph. */
  private def deleteNode(node: Node) {
    graph.remove(node).foreach(neighbours => neighbours.foreach { graph(_).remove(node.showingId) })
  }
}

object ConflictGraph {

  class Node(val showingId: Long) {

    def canEqual(other: Any): Boolean = other.isInstanceOf[Node]

    override def equals(other: Any): Boolean = other match {
      case that: Node => that.canEqual(this) && this.showingId == that.showingId
      case _ => false
    }

    override def hashCode: Int = showingId.hashCode

    override def toString: String = "N"+showingId
  }
}
