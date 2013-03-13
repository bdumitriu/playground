package org.ffplanner

import `def`.{ConstraintDefinition, ScheduleConstraints, FestivalProgrammeDefinition}
import scala.collection.JavaConversions.seqAsJavaList


/**
 *
 *
 * @author Bogdan Dumitriu
 */
class ScheduleBuilder(val festivalProgrammeDefinition: FestivalProgrammeDefinition) {

  val festivalProgramme: FestivalProgramme = new FestivalProgramme(
    Option(festivalProgrammeDefinition).getOrElse(FestivalProgrammeDefinition.EMPTY))

  def getPossibleSchedulesJ(scheduleConstraints: ScheduleConstraints): java.util.List[Schedule] =
    getPossibleSchedules(scheduleConstraints)

  def getPossibleSchedules(scheduleConstraints: ScheduleConstraints): List[Schedule] =
    new ScheduleCreator(this, scheduleConstraints).getSchedules
}
