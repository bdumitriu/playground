package org.ffplanner

import `def`.ConstraintDefinition.WatchType._
import `def`.{ScheduleDefinition, FestivalProgrammeDefinition}
import scala.collection.JavaConversions.seqAsJavaList
import scala.collection.JavaConversions.collectionAsScalaIterable

/**
 *
 *
 * @author Bogdan Dumitriu
 */
class ScheduleBuilder(festivalProgrammeDefinition: FestivalProgrammeDefinition) {

  val festivalProgramme: FestivalProgramme = new FestivalProgramme(festivalProgrammeDefinition)

  def getPossibleSchedulesJ(scheduleDefinition: ScheduleDefinition): java.util.List[Schedule] = {
    getPossibleSchedules(scheduleDefinition)
  }

  def getPossibleSchedules(scheduleDefinition: ScheduleDefinition): List[Schedule] = {
    val showingIds: List[Long] = scheduleDefinition.getShowingIds.toList.map(Long2long(_))
    val movieOnlyShowingIds: List[Long] = showingIds.filter(scheduleDefinition.getConstraint(_).getWatchType == MOVIE)
    List(
      new Schedule(movieOnlyShowingIds.map(festivalProgramme.getMovie(_)).map(festivalProgramme.showingsOf(_)(0).id), List.empty)
    )
  }
}
