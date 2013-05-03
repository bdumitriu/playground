package org.ffplanner

import org.ffplanner.ConflictGraph.Node

/**
  *
  * @author Bogdan Dumitriu
  */
object DefaultPlanningStrategy extends PlanningStrategy[Double] {

  override def chooseShowing(conflictGraph: ConflictGraph[Double]): Option[Long] = {
    val candidates: Set[Node[Double]] = conflictGraph.getNodesWithNeighbourCount(1)
    if (candidates.isEmpty) {
      None
    } else {
      Some(candidates.head.showingId)
    }
  }
}
