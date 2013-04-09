package org.ffplanner

import org.ffplanner.io.ShowingConstraintsXmlParser

/** Schedule constraints for the [[org.ffplanner.TestFestivalProgrammes.tiff2012Programme tiff2012Programme]].
  *
  * @author Bogdan Dumitriu
  */
trait TestScheduleConstraints {

  val scheduleConstraints1 =
    new ScheduleConstraints(new ShowingConstraintsXmlParser("/schedule_constraints_01.xml").scheduleConstraints)
}
