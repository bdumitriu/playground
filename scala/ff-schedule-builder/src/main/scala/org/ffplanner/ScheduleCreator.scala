package org.ffplanner

import org.ffplanner.ConflictGraph.Node
import scala.collection.immutable.{Iterable, SortedSet}

/** Creates a schedule starting from the input `constraints`.
  *
  * @author Bogdan Dumitriu
  */
class ScheduleCreator(val scheduleBuilder: ScheduleBuilder, val scheduleConstraints: ScheduleConstraints)
  extends ScheduleOperations {

  val festivalProgramme: FestivalProgramme = scheduleBuilder.festivalProgramme

  /**
    * @return one or more proposed schedules.
    */
  def getSchedules: List[Schedule] = {
    def chooseShowing(conflictGraph: ConflictGraph): Option[Long] = {
      val candidates: Set[Node] = conflictGraph.getNodesWithNeighbourCount(1)
      if (candidates.isEmpty) {
        None
      } else {
        Some(candidates.head.showingId)
      }
    }

    def getSchedules0(conflictGraph: ConflictGraph, scheduledShowingIds: Set[Long]): Set[Long] = {
      val scheduleableShowing: Option[Node] = conflictGraph.getFirstIsolatedNode
      if (scheduleableShowing.isEmpty) {
        chooseShowing(conflictGraph) match {
          case Some(showingId) => {
            conflictGraph.updateWith(showingId)
            getSchedules0(conflictGraph, scheduledShowingIds + showingId)
          }
          case None => scheduledShowingIds
        }
      } else {
        conflictGraph.updateWith(scheduleableShowing.get.showingId)
        getSchedules0(conflictGraph, scheduledShowingIds + scheduleableShowing.get.showingId)
      }
    }

    def getRemainingMovieIds(scheduledShowingIds: Set[Long]): Set[Long] = {
      scheduleConstraints.movieConstraintIds -- scheduledShowingIds.map(festivalProgramme.getShowing(_).movie.id)
    }

    val conflictGraph: ConflictGraph = new ConflictGraph(festivalProgramme)
    conflictGraph.initializeWith(scheduleConstraints)
    val scheduledShowingIds: Set[Long] = getSchedules0(conflictGraph, scheduleConstraints.showingConstraintIds)
    List(new Schedule(scheduledShowingIds, getRemainingMovieIds(scheduledShowingIds)))
  }

  /**
   * @return a range set covering the intervals of all the showings imposed via the `showingsConstraints`.
   */
  def getShowingsIntervals = getIntervalsOf(scheduleConstraints.showingConstraintIds, festivalProgramme)
}
