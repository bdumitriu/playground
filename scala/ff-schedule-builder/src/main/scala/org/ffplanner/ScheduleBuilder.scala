package org.ffplanner

import `def`.{ScheduleDefinition, FestivalProgrammeDefinition}
import scala.collection.JavaConversions.seqAsJavaList
import scala.collection.JavaConversions.collectionAsScalaIterable
import ScheduleDefinition.WatchType

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
    val showingIds: List[Long] = scheduleDefinition.getShowingIds(WatchType.MOVIE).toList.map(Long2long(_))
    List(
      new Schedule(showingIds.map(festivalProgramme.getMovie(_)).map(festivalProgramme.showingsOf(_)(0).id), List.empty)
    )
  }
}
