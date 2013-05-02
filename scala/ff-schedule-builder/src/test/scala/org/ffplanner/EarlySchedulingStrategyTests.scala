package org.ffplanner

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.ffplanner.`def`.FestivalProgrammeDefinition
import org.ffplanner.TestFestivalProgrammes._
import org.ffplanner.TestScheduleConstraints._

/** Unit tests for ensuring that the earliest scheduling possibilities are used.
  *
  * @author Bogdan Dumitriu
  */
@RunWith(classOf[JUnitRunner])
class EarlySchedulingStrategyTests extends FunSuite with ShouldMatchers {

  trait Tiff2012Fixture {
    val festivalProgrammeDefinition: FestivalProgrammeDefinition = tiff2012ProgrammeDefinition
    val scheduleBuilder: ScheduleBuilder = new ScheduleBuilder(festivalProgrammeDefinition)
  }

  test("earlier showing scheduled even if in conflict (showing 8)") {
    new Tiff2012Fixture {
      val schedules: List[Schedule] = scheduleBuilder.getPossibleSchedules(scheduleConstraints1)
      schedules should have size (1)
      schedules(0).showingIds should be (Set(1, 5, 8, 16, 96, 231))
    }
  }
}
