package org.ffplanner.strategy

import org.ffplanner.ConflictGraph.Node
import org.ffplanner.ConflictGraph
import scala.math
import org.ffplanner.ConflictGraph.Node.NodeOrdering
import scala.collection.immutable.SortedSet

/**
  *
  * @author Bogdan Dumitriu
  */
object DefaultPlanningStrategy extends PlanningStrategy[Score] {

  override def chooseShowing(conflictGraph: ConflictGraph[Score]): Option[Long] = {
    computeAbsoluteScores(conflictGraph)
    computeRelativeScores(conflictGraph)

    val candidates: SortedSet[Node[Score]] = conflictGraph.getNodesSortedBy(ScoreNodeOrdering)
    if (candidates.isEmpty) {
      None
    } else {
      Some(candidates.head.showingId)
    }
  }

  def computeAbsoluteScores(conflictGraph: ConflictGraph[Score]) {
    conflictGraph.visit { node: Node[Score] =>
      val nrNeighbours: Int = conflictGraph.numberOfNeighboursOf(node)
      val nrShowings: Int = conflictGraph.festivalProgramme.showingsOf(node.movieId).size
      node.data = new Score(1.0 / nrNeighbours + 1.0 / nrShowings, 0.0)
    }
  }

  def computeRelativeScores(conflictGraph: ConflictGraph[Score]) {
    conflictGraph.visit { node: Node[Score] =>
        val neighbours: Set[Node[Score]] = conflictGraph.neighboursOf(node)
        val totalScore = (neighbours + node).map(_.data.absolute).sum
        node.data.relative = node.data.absolute / totalScore
    }
  }
}

class Score(var absolute: Double, var relative: Double)

object ScoreNodeOrdering extends Ordering[Node[Score]] {

  private object DefaultNodeOrdering extends NodeOrdering[Score]

  def compare(node1: Node[Score], node2: Node[Score]): Int = {
    val scoreComparison: Int = math.Ordering.Double.compare(node1.data.relative, node2.data.relative)
    if (scoreComparison == 0) {
      DefaultNodeOrdering.compare(node1, node2)
    } else {
      scoreComparison
    }
  }
}
