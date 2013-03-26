package org.ffplanner

import `def`._
import org.joda.time.{Period, DateTime}
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import com.google.common.collect.{TreeRangeSet, RangeSet}
import org.scalatest.matchers.ShouldMatchers

/**
 *
 *
 * @author Bogdan Dumitriu
 */
@RunWith(classOf[JUnitRunner])
class ScheduleTests extends FunSuite with ShouldMatchers {

  trait TestFixture1 {

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

  trait TestFixture2 {

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
    new TestFixture1 {
      val scheduleConstraints: TestScheduleConstraints = new TestScheduleConstraints(Set.empty, Set.empty)
      assert(scheduleBuilder.getPossibleSchedules(scheduleConstraints)(0).showingIds.size === 0)
    }
  }

  test("one movie") {
    new TestFixture1 {
      val scheduleConstraints: TestScheduleConstraints = new TestScheduleConstraints(
        Set[TestMovieConstraint](new TestMovieConstraint(10, 2)),
        Set[TestShowingConstraint]())
      val schedules: List[Schedule] = scheduleBuilder.getPossibleSchedules(scheduleConstraints)
      assert(schedules.size === 1)
      assert(schedules(0).showingIds.head == 15 || schedules(0).showingIds.head == 25, "Non existing showing id selected.")
    }
  }

  test("no overlap") {
    new TestFixture2 {
      val scheduleConstraints: TestScheduleConstraints = new TestScheduleConstraints(
        Set[TestMovieConstraint](
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
        Set[TestShowingConstraint]()
      )
      val schedules: List[Schedule] = scheduleBuilder.getPossibleSchedules(scheduleConstraints)

      for (schedule <- schedules) {
        assertBasicScheduleInvariants(schedule, scheduleConstraints, scheduleBuilder.festivalProgramme)
      }
    }
  }

  test("showings have priority over movies") {
    new TestFixture2 {
      val scheduleConstraints: TestScheduleConstraints = new TestScheduleConstraints(
        Set[TestMovieConstraint](
          new TestMovieConstraint(1, 2),
          new TestMovieConstraint(2, 2),
          new TestMovieConstraint(4, 2),
          new TestMovieConstraint(5, 2),
          new TestMovieConstraint(6, 2),
          new TestMovieConstraint(8, 2),
          new TestMovieConstraint(9, 2),
          new TestMovieConstraint(10, 2),
          new TestMovieConstraint(12, 2)
        ),
        Set[TestShowingConstraint](
          new TestShowingConstraint(1003, 2),
          new TestShowingConstraint(1007, 2),
          new TestShowingConstraint(1011, 2)
        )
      )
      val schedules: List[Schedule] = scheduleBuilder.getPossibleSchedules(scheduleConstraints)

      for (schedule <- schedules) {
        assertBasicScheduleInvariants(schedule, scheduleConstraints, scheduleBuilder.festivalProgramme)
      }
    }
  }

  private def assertBasicScheduleInvariants(
    schedule: Schedule, scheduleConstraints: TestScheduleConstraints, festivalProgramme: FestivalProgramme) {
    assertNoOverlap(schedule, festivalProgramme)
    assertAllShowingsScheduled(schedule, scheduleConstraints, festivalProgramme)
    assertNoMissedSchedulingOpportunities(schedule, scheduleConstraints, festivalProgramme)
    assertMissedMoviesComputedCorrectly(schedule, scheduleConstraints, festivalProgramme)
  }

  private def assertNoOverlap(schedule: Schedule, festivalProgramme: FestivalProgramme) {
    val intervals: RangeSet[DateTime] = TreeRangeSet.create[DateTime]()
    for (showingId <- schedule.showingIds) {
      val showing = festivalProgramme.getShowing(showingId)
      assert(!showing.overlapsWith(intervals), "Scheduled showing\n\t\t"+showing+"overlaps another scheduled showing.")
      intervals.add(showing.interval)
    }
  }

  private def assertAllShowingsScheduled(
    schedule: Schedule, scheduleConstraints: TestScheduleConstraints, festivalProgramme: FestivalProgramme) {
    val showingIds: Set[Long] = schedule.showingIds
    for (showingConstraint <- scheduleConstraints.showingConstraints) {
      showingIds should contain (showingConstraint.showingId)
    }
  }

  private def assertNoMissedSchedulingOpportunities(
    schedule: Schedule, scheduleConstraints: TestScheduleConstraints, festivalProgramme: FestivalProgramme) {
    val intervals: RangeSet[DateTime] = schedule.getShowingsIntervals(festivalProgramme)
    for (missedMovieId <- schedule.missedMovieIds) {
      for (movieShowing <- festivalProgramme.showingsOf(missedMovieId)) {
        assert(movieShowing.overlapsWith(intervals) || !movieShowing.within(scheduleConstraints),
          "Unscheduled movie "+missedMovieId+" could have been scheduled in the slot:\n\t\t"+movieShowing)
      }
    }
  }

  def assertMissedMoviesComputedCorrectly(
    schedule: Schedule, scheduleConstraints: TestScheduleConstraints, festivalProgramme: FestivalProgramme) {
    assertNoScheduledShowingInMissedMovies(schedule)
    assertNoMissedMovieInScheduledShowings(schedule, festivalProgramme)
    assertAllMovieConstraintsEitherScheduledOrUnscheduled(schedule, scheduleConstraints, festivalProgramme)
  }

  def assertNoScheduledShowingInMissedMovies(schedule: Schedule) {
    for (showingId <- schedule.showingIds) {
      schedule.missedMovieIds should not contain (showingId)
    }
  }

  def assertNoMissedMovieInScheduledShowings(schedule: Schedule, festivalProgramme: FestivalProgramme) {
    for (missedMovieId <- schedule.missedMovieIds) {
      for (movieShowing <- festivalProgramme.showingsOf(missedMovieId)) {
        schedule.showingIds should not contain (movieShowing.id)
      }
    }
  }

  def assertAllMovieConstraintsEitherScheduledOrUnscheduled(
    schedule: Schedule, scheduleConstraints: TestScheduleConstraints, festivalProgramme: FestivalProgramme) {
    val movieIdConstraints = scheduleConstraints.movieConstraints.map(_.movieId) --
      schedule.showingIds.map(festivalProgramme.getShowing(_).movie.id) --
      schedule.missedMovieIds
    movieIdConstraints should be ('empty)
  }
}
