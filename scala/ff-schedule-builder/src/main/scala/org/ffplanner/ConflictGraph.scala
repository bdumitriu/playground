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
class ConflictGraph[T](val festivalProgramme: FestivalProgramme) {

  private val graphNodes: mutable.Map[Long, Node[T]] = mutable.Map.empty

  private val graph: mutable.Map[Node[T], mutable.Set[Node[T]]] = mutable.Map.empty.withDefaultValue(mutable.Set.empty)

  private implicit object NodeOrderingT extends NodeOrdering[T]

  private def getOrBuildNodeFor(showing: Showing, priority: Short): Node[T] = {
    graphNodes.get(showing.id).getOrElse {
      val node = new Node[T](new ShowingConstraint(showing, priority))
      graphNodes(showing.id) = node
      node
    }
  }

  /** Initializes the graph such that all scheduleable showings of the movies in `scheduleConstraints` are added. A
    * showing is not scheduleable if it conflicts with the showings in `scheduleConstraints`.
    */
  def initializeWith(scheduleConstraints: ScheduleConstraints) {
    def createGraphNodes(movieConstraint: MovieConstraint) {
      val movieShowings =  festivalProgramme.showingsOf(movieConstraint.movie).filter { showing: Showing =>
        festivalProgramme.getConflictsOf(showing.id, Set.empty, scheduleConstraints.showingConstraintIds).isEmpty
      }
      movieShowings.foreach(getOrBuildNodeFor(_, movieConstraint.priority))
    }

    def addShowingToGraph(node: Node[T]) {
      // we cannot have conflicts with scheduleConstraints.showingConstraintIds because nodes that had such conflicts
      // have already been eliminated in createGraphNodes
      val conflicts: Set[Long] =
        festivalProgramme.getConflictsOf(node.showingId, scheduleConstraints.movieConstraintIds, Set.empty)
      graph(node) = mutable.Set(conflicts.toList: _*).filter(graphNodes.get(_).isDefined).map(graphNodes)
    }

    graph.clear()
    scheduleConstraints.movieConstraints.foreach(createGraphNodes)
    graphNodes.values.foreach(addShowingToGraph)
  }

  /** Registers the scheduling of a showing. The showing, all its direct neighbours and all the other showings of the
    * same movie will be deleted.
    */
  def updateWith(scheduledShowing: Showing) {
    def deleteNodeAndNeighbours(showing: Showing) {
      graph.remove(graphNodes.get(showing.id).getOrElse(null)).foreach(neighbours => neighbours.foreach(deleteNode))
    }

    deleteNodeAndNeighbours(scheduledShowing)
    (festivalProgramme.showingsOf(scheduledShowing.movie) - scheduledShowing).foreach { showing: Showing =>
      graphNodes.get(showing.id).map(deleteNode)
    }
  }

  /** Registers the fact that a showing has been scheduled. The showing, all its direct neighbours and all the other
    * showings of the same movie as well as their direct neighbours will be deleted.
    */
  def updateWith(scheduledShowingId: Long) {
    updateWith(festivalProgramme.getShowing(scheduledShowingId))
  }

  def hasShowing(showingId: Long): Boolean = graphNodes.get(showingId).map(graph.contains).getOrElse(false)

  def neighboursOf(showingId: Long): Set[Node[T]] = graphNodes.get(showingId).map(graph(_).toSet).getOrElse(Set.empty)

  def neighboursOf(node: Node[T]): Set[Node[T]] = graph.get(node).map(_.toSet).getOrElse(Set.empty)

  def numberOfNeighboursOf(node: Node[T]) = graph.get(node).map(_.size).getOrElse(0)

  def getFirstIsolatedNode: Option[Node[T]] = getIsolatedNodes.headOption

  /**
    * @return the isolated nodes in the graph, grouped by movie id.
    */
  def getIsolatedNodes: SortedSet[Node[T]] = getNodesWithNeighbourCount(0)

  def getNodesWithNeighbourCount(nrNeighbours: Int): SortedSet[Node[T]] =
    SortedSet.empty[Node[T]](NodeOrderingT) ++ graph.keySet filter { graph(_).size == nrNeighbours }

  def getNodesSortedBy(ordering: Ordering[Node[T]]): SortedSet[Node[T]] =
    SortedSet.empty[Node[T]](ordering) ++ graph.keySet

  def visit(visitNode: Node[T] => Unit) {
    graph.keySet foreach visitNode
  }

  /** Deletes the `node` and all the edges leading to it form the graph. */
  private def deleteNode(node: Node[T]) {
    graph.remove(node).foreach(neighbours => neighbours.foreach { graph(_).remove(node) })
  }
}

object ConflictGraph {

  class Node[T](val showingConstraint: ShowingConstraint) {

    var data: T = null.asInstanceOf[T]

    def showingId: Long = showingConstraint.showing.id

    def movieId: Long = showingConstraint.showing.movie.id

    def priority: Short = showingConstraint.priority

    def canEqual(other: Any): Boolean = other.isInstanceOf[Node[T]]

    override def equals(other: Any): Boolean = other match {
      case that: Node[T] => that.canEqual(this) && this.showingConstraint.showing == that.showingConstraint.showing
      case _ => false
    }

    override def hashCode: Int = showingConstraint.showing.hashCode

    override def toString: String = "N"+showingId
  }

  object Node {

    trait NodeOrdering[T] extends Ordering[Node[T]] {
      def compare(node1: Node[T], node2: Node[T]): Int = {
        ShowingOrdering.compare(node1.showingConstraint.showing, node2.showingConstraint.showing)
      }
    }
  }
}
