package org.ffplanner

import org.ffplanner.io.ShowingConstraintsXmlParser
import TestFestivalProgrammes.tiff2012Programme
import Utils.DefaultPriority

/** Schedule constraints for the [[org.ffplanner.TestFestivalProgrammes.tiff2012Programme tiff2012Programme]].
  *
  * @author Bogdan Dumitriu
  */
object TestScheduleConstraints {

  val tiff2012AllMovieConstraints = new ScheduleConstraints(
    tiff2012Programme.getMovies.map(new MovieConstraint(_, DefaultPriority)), Set.empty[ShowingConstraint])

  val scheduleConstraints1 = new ScheduleConstraints(
    tiff2012Programme, new ShowingConstraintsXmlParser("/schedule_constraints_01.xml").scheduleConstraints)
}
