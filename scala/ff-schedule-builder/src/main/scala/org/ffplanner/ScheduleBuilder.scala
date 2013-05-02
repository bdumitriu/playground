package org.ffplanner

import `def`.{ScheduleConstraintsDefinition, FestivalProgrammeDefinition}
import scala.collection.JavaConversions.seqAsJavaList


/** The entry point to the scheduling library.
  *
  * @author Bogdan Dumitriu
  */
class ScheduleBuilder(val festivalProgrammeDefinition: FestivalProgrammeDefinition) {

  val festivalProgramme: FestivalProgramme = new FestivalProgramme(
    Option(festivalProgrammeDefinition).getOrElse(FestivalProgrammeDefinition.EMPTY))

  def getPossibleSchedulesJ(scheduleConstraints: ScheduleConstraintsDefinition): java.util.List[Schedule] =
    getPossibleSchedules(scheduleConstraints)

  def getPossibleSchedules(scheduleConstraints: ScheduleConstraintsDefinition): List[Schedule] =
    getPossibleSchedules(new ScheduleConstraints(festivalProgramme, scheduleConstraints))

  def getPossibleSchedules(scheduleConstraints: ScheduleConstraints): List[Schedule] =
    new ScheduleCreator(this, scheduleConstraints).getSchedules
}
