package org.ffplanner

import `def`._
import org.joda.time.DateTime
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 *
 *
 * @author Bogdan Dumitriu
 */
@RunWith(classOf[JUnitRunner])
class ScheduleTests extends FunSuite {

  trait TestScheduleDefinitions {
    val festivalProgramme: FestivalProgrammeDefinition = new FestivalProgrammeDefinition {
      def getShowings: java.util.List[ShowingDefinition] = {
        val showings: java.util.LinkedList[ShowingDefinition] = new java.util.LinkedList[ShowingDefinition]()
        showings.add(new Showing(15, DateTime.now, new Movie(10)))
        showings.add(new Showing(25, DateTime.now, new Movie(10)))
        showings
      }
    }
    val scheduleDefinition1: ScheduleDefinition = new TestScheduleDefinition(List.empty, List.empty)
    val scheduleDefinition2: ScheduleDefinition = new TestScheduleDefinition(
      List[ConstraintDefinition.Movie](new TestMovieConstraint(10, 2)),
      List[ConstraintDefinition.Showing]())
  }

  test("no movies") {
    new TestScheduleDefinitions {
      assert(new ScheduleBuilder(festivalProgramme).getPossibleSchedules(scheduleDefinition1)(0).showings.size === 0)
    }
  }

  test("one definitely movie") {
    new TestScheduleDefinitions {
      val schedules: List[Schedule] = new ScheduleBuilder(festivalProgramme).getPossibleSchedules(scheduleDefinition2)
      assert(schedules.size === 1)
      assert(schedules(0).showings(0) === 15)
    }
  }
}
