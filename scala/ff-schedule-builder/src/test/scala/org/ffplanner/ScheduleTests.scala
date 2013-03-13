package org.ffplanner

import `def`._
import org.joda.time.{DateTimeConstants, Period, DateTime}
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import com.google.common.collect.{TreeRangeSet, RangeSet}

/**
 *
 *
 * @author Bogdan Dumitriu
 */
@RunWith(classOf[JUnitRunner])
class ScheduleTests extends FunSuite {

  trait TestSuite1 {

    val festivalProgrammeDefinition: FestivalProgrammeDefinition = new FestivalProgrammeDefinition {
      def getShowings: java.util.List[ShowingDefinition] = {
        val showings: java.util.LinkedList[ShowingDefinition] = new java.util.LinkedList[ShowingDefinition]()
        showings.add(new TestShowing(15, 100, DateTime.now, new TestMovie(10, Period.minutes(60))))
        showings.add(new TestShowing(25, 100, DateTime.now, new TestMovie(10, Period.minutes(60))))
        showings
      }
    }

    val scheduleBuilder: ScheduleBuilder = new ScheduleBuilder(festivalProgrammeDefinition)
  }

  trait TestSuite2 {

    val festivalProgrammeDefinition: FestivalProgrammeDefinition = new FestivalProgrammeDefinition {

      def getShowings: java.util.List[ShowingDefinition] = {
        val movies: Map[Long, TestMovie] = List(
          new TestMovie(1, Period.minutes(76)),
          new TestMovie(2, Period.minutes(104)),
          new TestMovie(3, Period.minutes(99)),
          new TestMovie(4, Period.minutes(121)),
          new TestMovie(5, Period.minutes(93)),
          new TestMovie(6, Period.minutes(76)),
          new TestMovie(7, Period.minutes(135)),
          new TestMovie(8, Period.minutes(105)),
          new TestMovie(9, Period.minutes(84)),
          new TestMovie(10, Period.minutes(97)),
          new TestMovie(11, Period.minutes(116)),
          new TestMovie(12, Period.minutes(88))
        ).map(m => (m.id, m)).toMap

        val showings: java.util.LinkedList[ShowingDefinition] = new java.util.LinkedList[ShowingDefinition]()
        showings.add(new TestShowing(1001, 100, DateTime.parse("2012-06-01T11:00"), movies(1)))
        showings.add(new TestShowing(1002, 100, DateTime.parse("2012-06-01T13:00"), movies(2)))
        showings.add(new TestShowing(1003, 100, DateTime.parse("2012-06-01T15:30"), movies(3)))
        showings.add(new TestShowing(1004, 100, DateTime.parse("2012-06-01T18:00"), movies(4)))
        showings.add(new TestShowing(1005, 100, DateTime.parse("2012-06-01T21:00"), movies(5)))
        showings.add(new TestShowing(1006, 100, DateTime.parse("2012-06-01T23:00"), movies(6)))
        showings.add(new TestShowing(1007, 101, DateTime.parse("2012-06-01T10:00"), movies(7)))
        showings.add(new TestShowing(1008, 101, DateTime.parse("2012-06-01T12:30"), movies(8)))
        showings.add(new TestShowing(1009, 101, DateTime.parse("2012-06-01T15:00"), movies(9)))
        showings.add(new TestShowing(1010, 101, DateTime.parse("2012-06-01T17:30"), movies(10)))
        showings.add(new TestShowing(1011, 101, DateTime.parse("2012-06-01T20:00"), movies(11)))
        showings.add(new TestShowing(1012, 101, DateTime.parse("2012-06-01T22:30"), movies(12)))
        showings
      }
    }

    val scheduleBuilder: ScheduleBuilder = new ScheduleBuilder(festivalProgrammeDefinition)
  }

  test("no movies") {
    new TestSuite1 {
      val scheduleConstraints: ScheduleConstraints = new TestScheduleConstraints(List.empty, List.empty)
      assert(scheduleBuilder.getPossibleSchedules(scheduleConstraints)(0).showingIds.size === 0)
    }
  }

  test("one movie") {
    new TestSuite1 {
      val scheduleConstraints: ScheduleConstraints = new TestScheduleConstraints(
        List[ConstraintDefinition.Movie](new TestMovieConstraint(10, 2)),
        List[ConstraintDefinition.Showing]())
      val schedules: List[Schedule] = scheduleBuilder.getPossibleSchedules(scheduleConstraints)
      assert(schedules.size === 1)
      assert(schedules(0).showingIds(0) == 15 || schedules(0).showingIds(0) == 25, "Non existing showing id selected.")
    }
  }

  test("no overlap") {
    new TestSuite2 {
      val scheduleConstraints: ScheduleConstraints = new TestScheduleConstraints(
        List[ConstraintDefinition.Movie](
          new TestMovieConstraint(1, 2),
          new TestMovieConstraint(2, 2),
          new TestMovieConstraint(3, 2),
          new TestMovieConstraint(4, 2),
          new TestMovieConstraint(5, 2),
          new TestMovieConstraint(6, 2),
          new TestMovieConstraint(7, 2),
          new TestMovieConstraint(8, 2),
          new TestMovieConstraint(9, 2),
          new TestMovieConstraint(10, 2),
          new TestMovieConstraint(11, 2),
          new TestMovieConstraint(12, 2)
        ),
        List[ConstraintDefinition.Showing]()
      )
      val schedules: List[Schedule] = scheduleBuilder.getPossibleSchedules(scheduleConstraints)

      for (schedule <- schedules) {
        assertNoOverlap(schedule, scheduleBuilder.festivalProgramme)
      }
    }
  }

  def assertNoOverlap(schedule: Schedule, festivalProgramme: FestivalProgramme) {
    val rangeSet: RangeSet[DateTime] = TreeRangeSet.create[DateTime]()
    for (showingId <- schedule.showingIds) {
      val showing = festivalProgramme.getShowing(showingId)
      val showingInterval = showing.interval
      assert(rangeSet.subRangeSet(showingInterval).isEmpty, "Scheduled showing "+showingId+
        " overlaps another scheduled showing:\n\t\t"+showingInterval+" @ venue "+showing.venueId)
      rangeSet.add(showingInterval)
    }
  }
}
