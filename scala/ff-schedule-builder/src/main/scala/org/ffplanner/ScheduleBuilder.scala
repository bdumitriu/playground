package org.ffplanner

import `def`.{ConstraintDefinition, ScheduleDefinition, FestivalProgrammeDefinition}
import scala.collection.JavaConversions.seqAsJavaList


/**
 *
 *
 * @author Bogdan Dumitriu
 */
class ScheduleBuilder(val festivalProgrammeDefinition: FestivalProgrammeDefinition) {

  val festivalProgramme: FestivalProgramme = new FestivalProgramme(
    Option(festivalProgrammeDefinition).getOrElse(FestivalProgrammeDefinition.EMPTY))

  def getPossibleSchedulesJ(scheduleDefinition: ScheduleDefinition): java.util.List[Schedule] = {
    getPossibleSchedules(scheduleDefinition)
  }

  def getPossibleSchedules(scheduleDefinition: ScheduleDefinition): List[Schedule] = {
    val scheduleDef: ScheduleDefinition = Option(scheduleDefinition).getOrElse(ScheduleDefinition.EMPTY)
    val movieConstraints: List[ConstraintDefinition.Movie] = Utils.ensureNonNull(scheduleDef.getMovieConstraints)
    val showingConstraints: List[ConstraintDefinition.Showing] = Utils.ensureNonNull(scheduleDef.getShowingConstraints)
    val movieIds: List[Long] = movieConstraints map { s => Long2long(s.getMovieId) }
    val showingIds: List[Long] = showingConstraints map { s => Long2long(s.getShowingId) }
    List(
      new Schedule(movieIds.map(new Movie(_)).map(festivalProgramme.showingsOf(_)(0).id), List.empty)
    )
  }
}
