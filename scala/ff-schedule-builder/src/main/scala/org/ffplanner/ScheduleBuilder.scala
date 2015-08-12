package org.ffplanner

import `def`.{ScheduleConstraintsDefinition, FestivalProgrammeDefinition}
import scala.collection.JavaConversions.seqAsJavaList
import org.ffplanner.strategy.DefaultPlanningStrategy


/** The entry point to the scheduling library.
  *
  * [[org.ffplanner.ScheduleBuilder.getPossibleSchedulesJ]] will return a list of proposed schedules that satisfy the
  * supplied `scheduleConstraints`. Showing constraints will '''not''' be included in the proposed schedules, but they
  * are taken into account when generating the proposed schedules.
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
    new ScheduleCreator(this, scheduleConstraints).getSchedules(DefaultPlanningStrategy)
}
