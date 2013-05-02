package org.ffplanner

import scala.language.implicitConversions
import collection.mutable
import org.ffplanner.ConflictGraph.Node
import scala.collection.immutable.SortedSet
import org.ffplanner.Showing.ShowingOrdering
import org.ffplanner.ConflictGraph.Node.NodeOrdering

/** A graph whose nodes contain showings (of a movie). An edge between two showing nodes indicates that the two are in
  * conflict.
  *
  * @author Bogdan Dumitriu
  */
class ConflictGraph(val festivalProgramme: FestivalProgramme) {

  private val graphNodes: mutable.Map[Long, Node] = mutable.Map.empty

  private val graph: mutable.Map[Node, mutable.Set[Node]] = mutable.Map.empty.withDefaultValue(mutable.Set.empty)

  private implicit def graphNodeFor(showing: Showing): Node = graphNodes(showing.id)

  private def getOrBuildNodeFor(showing: Showing, priority: Short): Node = {
    graphNodes.get(showing.id).getOrElse {
      val node = new Node(new ShowingConstraint(showing, priority))
      graphNodes(showing.id) = node
      node
    }
  }

  /** Initializes the graph such that all scheduleable showings of the movies in `scheduleConstraints` are added. A
    * showing is not scheduleable if it conflicts with the showings in `scheduleConstraints`.
    */
  def initializeWith(scheduleConstraints: ScheduleConstraints) {
    def createGraphNodes(movieConstraint: MovieConstraint) {
      festivalProgramme.showingsOf(movieConstraint.movie).foreach(getOrBuildNodeFor(_, movieConstraint.priority))
    }

    def addMovieShowingsToGraph(movieConstraint: MovieConstraint) {
      festivalProgramme.showingsOf(movieConstraint.movie).foreach(addShowingToGraph(_, movieConstraint.priority))
    }

    def addShowingToGraph(showing: Showing, priority: Short) {
      val conflicts: Set[Long] = festivalProgramme.getConflictsOf(
        showing.id, scheduleConstraints.movieConstraintIds, scheduleConstraints.showingConstraintIds)
      if (conflicts.intersect(scheduleConstraints.showingConstraintIds).isEmpty) {
        graph(showing) = mutable.Set(conflicts.toList: _*).map(festivalProgramme.getShowing _ andThen graphNodeFor)
      }
    }

    graph.clear()
    scheduleConstraints.movieConstraints.foreach(createGraphNodes)
    scheduleConstraints.movieConstraints.foreach(addMovieShowingsToGraph)
  }

  /** Registers the scheduling of a showing. The showing, all its direct neighbours and all the other showings of the
    * same movie will be deleted.
    */
  def updateWith(scheduledShowing: Showing) {
    def deleteNodeAndNeighbours(showing: Showing) {
      graph.remove(showing).foreach(neighbours => neighbours.foreach(deleteNode))
    }

    deleteNodeAndNeighbours(scheduledShowing)
    (festivalProgramme.showingsOf(scheduledShowing.movie) - scheduledShowing).foreach(graphNodeFor _ andThen deleteNode)
  }

  /** Registers the fact that a showing has been scheduled. The showing, all its direct neighbours and all the other
    * showings of the same movie as well as their direct neighbours will be deleted.
    */
  def updateWith(scheduledShowingId: Long) {
    updateWith(festivalProgramme.getShowing(scheduledShowingId))
  }

  def hasShowing(showingId: Long): Boolean = graphNodes.get(showingId).map(graph.contains).getOrElse(false)

  def neighboursOf(showingId: Long): Set[Node] = graphNodes.get(showingId).map(graph(_).toSet).getOrElse(Set.empty)

  def getFirstIsolatedNode: Option[Node] = getIsolatedNodes.headOption

  /**
    * @return the isolated nodes in the graph, grouped by movie id.
    */
  def getIsolatedNodes: SortedSet[Node] = getNodesWithNeighbourCount(0)

  def getNodesWithNeighbourCount(nrNeighbours: Int): SortedSet[Node] =
    SortedSet.empty[Node](NodeOrdering) ++ graph.keySet filter { graph(_).size == nrNeighbours }

  /** Deletes the `node` and all the edges leading to it form the graph. */
  private def deleteNode(node: Node) {
    graph.remove(node).foreach(neighbours => neighbours.foreach { graph(_).remove(node) })
  }
}

object ConflictGraph {

  class Node(val showingConstraint: ShowingConstraint) {

    def showingId: Long = showingConstraint.showing.id

    def movieId: Long = showingConstraint.showing.movie.id

    def priority: Short = showingConstraint.priority

    def canEqual(other: Any): Boolean = other.isInstanceOf[Node]

    override def equals(other: Any): Boolean = other match {
      case that: Node => that.canEqual(this) && this.showingConstraint.showing == that.showingConstraint.showing
      case _ => false
    }

    override def hashCode: Int = showingConstraint.showing.hashCode

    override def toString: String = "N"+showingId
  }

  object Node {

    implicit object NodeOrdering extends Ordering[Node] {
      def compare(node1: Node, node2: Node): Int = {
        ShowingOrdering.compare(node1.showingConstraint.showing, node2.showingConstraint.showing)
      }
    }
  }
}
