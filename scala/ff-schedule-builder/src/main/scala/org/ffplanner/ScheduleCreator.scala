package org.ffplanner

import org.ffplanner.ConflictGraph.Node
import scala.collection.immutable.{Iterable, SortedSet}
import org.ffplanner.strategy.PlanningStrategy

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
  def getSchedules[T](planningStrategy: PlanningStrategy[T]): List[Schedule] = {
    def getSchedules0(conflictGraph: ConflictGraph[T], scheduledShowingIds: Set[Long]): Set[Long] = {
      planningStrategy.chooseNextShowing(conflictGraph) match {
        case Some(showingId) => {
          conflictGraph.updateWith(showingId)
          getSchedules0(conflictGraph, scheduledShowingIds + showingId)
        }
        case None => scheduledShowingIds
      }
    }

    def getRemainingMovieIds(scheduledShowingIds: Set[Long]): Set[Long] = {
      scheduleConstraints.movieConstraintIds -- scheduledShowingIds.map(festivalProgramme.getShowing(_).movie.id)
    }

    val conflictGraph: ConflictGraph[T] = new ConflictGraph[T](festivalProgramme)
    conflictGraph.initializeWith(scheduleConstraints)
    val scheduledShowingIds: Set[Long] = getSchedules0(conflictGraph, scheduleConstraints.showingConstraintIds)
    List(new Schedule(scheduledShowingIds, getRemainingMovieIds(scheduledShowingIds)))
  }

  /**
   * @return a range set covering the intervals of all the showings imposed via the `showingsConstraints`.
   */
  def getShowingsIntervals = getIntervalsOf(scheduleConstraints.showingConstraintIds, festivalProgramme)
}
