package org.ffplanner

import org.ffplanner.ConflictGraph.Node

/** Provides a strategy for choosing which showing to schedule next in a given
  * [[org.ffplanner.ConflictGraph ConflictGraph]] configuration.
  *
  * @author Bogdan Dumitriu
  */
trait PlanningStrategy[T] {

  def chooseNextShowing(conflictGraph: ConflictGraph[T]): Option[Long] = {
    conflictGraph.getFirstIsolatedNode match {
      case Some(node) => Some(node.showingId)
      case None => chooseShowing(conflictGraph)
    }
  }

  /** Choose the next showing when there is no isolated node (i.e., node with no conflict) that can be scheduled. */
  protected def chooseShowing(conflictGraph: ConflictGraph[T]): Option[Long]
}
