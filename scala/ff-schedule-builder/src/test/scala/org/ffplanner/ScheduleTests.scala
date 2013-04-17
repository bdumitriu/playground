package org.ffplanner

import `def`._
import org.joda.time.DateTime
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import com.google.common.collect.{TreeRangeSet, RangeSet}
import org.scalatest.matchers.ShouldMatchers
import org.ffplanner.TestFestivalProgrammes._

/** Unit tests for [[org.ffplanner.ScheduleCreator ScheduleCreator]].
  *
  * @author Bogdan Dumitriu
  */
@RunWith(classOf[JUnitRunner])
class ScheduleTests extends FunSuite with ShouldMatchers {

  trait TestFixture1 {
    val festivalProgrammeDefinition: FestivalProgrammeDefinition = festivalProgrammeDefinition1
    val scheduleBuilder: ScheduleBuilder = new ScheduleBuilder(festivalProgrammeDefinition)
  }

  trait TestFixture2 {
    val festivalProgrammeDefinition: FestivalProgrammeDefinition = festivalProgrammeDefinition2
    val scheduleBuilder: ScheduleBuilder = new ScheduleBuilder(festivalProgrammeDefinition)
  }

  trait TestFixture3 {
    val festivalProgrammeDefinition: FestivalProgrammeDefinition = festivalProgrammeDefinition3
    val scheduleBuilder: ScheduleBuilder = new ScheduleBuilder(festivalProgrammeDefinition)
  }

  trait Tiff2012Fixture {
    val festivalProgrammeDefinition: FestivalProgrammeDefinition = tiff2012ProgrammeDefinition
    val scheduleBuilder: ScheduleBuilder = new ScheduleBuilder(festivalProgrammeDefinition)
  }

  test("no movies") {
    new TestFixture1 {
      val scheduleConstraints: ScheduleConstraintsMock = new ScheduleConstraintsMock(Set.empty, Set.empty)
      val schedules: List[Schedule] = scheduleBuilder.getPossibleSchedules(scheduleConstraints)
      schedules should have size (1)
      schedules(0).showingIds should be ('empty)
    }
  }

  test("single movie") {
    new TestFixture1 {
      val scheduleConstraints: ScheduleConstraintsMock =
        new ScheduleConstraintsMock(Set(new MovieConstraintMock(10, 2)), Set())
      val schedules: List[Schedule] = scheduleBuilder.getPossibleSchedules(scheduleConstraints)
      schedules should have size (1)
      schedules(0).showingIds should have size (1)
      schedules(0).showingIds.head should be (15)
    }
  }

  test("no overlap") {
    new TestFixture2 {
      val scheduleConstraints: ScheduleConstraintsMock = new ScheduleConstraintsMock(
        Set[MovieConstraintMock](
          new MovieConstraintMock(1, 2),
          new MovieConstraintMock(2, 2),
          new MovieConstraintMock(3, 2),
          new MovieConstraintMock(4, 2),
          new MovieConstraintMock(5, 2),
          new MovieConstraintMock(6, 2),
          new MovieConstraintMock(7, 2),
          new MovieConstraintMock(8, 2),
          new MovieConstraintMock(9, 2),
          new MovieConstraintMock(10, 2),
          new MovieConstraintMock(11, 2),
          new MovieConstraintMock(12, 2)
        ),
        Set[ShowingConstraintMock]()
      )
      val schedules: List[Schedule] = scheduleBuilder.getPossibleSchedules(scheduleConstraints)

      for (schedule <- schedules) {
        assertBasicScheduleInvariants(schedule, scheduleConstraints, scheduleBuilder.festivalProgramme)
      }
    }
  }

  test("showings have priority over movies") {
    new TestFixture2 {
      val scheduleConstraints: ScheduleConstraintsMock = new ScheduleConstraintsMock(
        Set[MovieConstraintMock](
          new MovieConstraintMock(1, 2),
          new MovieConstraintMock(2, 2),
          new MovieConstraintMock(4, 2),
          new MovieConstraintMock(5, 2),
          new MovieConstraintMock(6, 2),
          new MovieConstraintMock(8, 2),
          new MovieConstraintMock(9, 2),
          new MovieConstraintMock(10, 2),
          new MovieConstraintMock(12, 2)
        ),
        Set[ShowingConstraintMock](
          new ShowingConstraintMock(1003, 2),
          new ShowingConstraintMock(1007, 2),
          new ShowingConstraintMock(1011, 2)
        )
      )
      val schedules: List[Schedule] = scheduleBuilder.getPossibleSchedules(scheduleConstraints)

      for (schedule <- schedules) {
        assertBasicScheduleInvariants(schedule, scheduleConstraints, scheduleBuilder.festivalProgramme)
      }
    }
  }

  test("optimal scheduling - no movie 3 in venue 1 leaves room for movie 9 in venue 2") {
    new TestFixture2 {
      val scheduleConstraints: ScheduleConstraintsMock = new ScheduleConstraintsMock(
        Set[MovieConstraintMock](
          new MovieConstraintMock(1, 2),
          new MovieConstraintMock(2, 2),
          new MovieConstraintMock(4, 2),
          new MovieConstraintMock(5, 2),
          new MovieConstraintMock(6, 2),
          new MovieConstraintMock(7, 2),
          new MovieConstraintMock(8, 2),
          new MovieConstraintMock(9, 2),
          new MovieConstraintMock(10, 2),
          new MovieConstraintMock(11, 2),
          new MovieConstraintMock(12, 2)
        ),
        Set[ShowingConstraintMock]()
      )
      val schedules: List[Schedule] = scheduleBuilder.getPossibleSchedules(scheduleConstraints)

      for (schedule <- schedules) {
        assertBasicScheduleInvariants(schedule, scheduleConstraints, scheduleBuilder.festivalProgramme)
      }
    }
  }

  test("optimal scheduling - movie 1 scheduled in second slot to leave room for movie 7 in the shared slot") {
    new TestFixture3 {
      val scheduleConstraints: ScheduleConstraintsMock = new ScheduleConstraintsMock(
        Set[MovieConstraintMock](
          new MovieConstraintMock(1, 2),
          new MovieConstraintMock(7, 2)
        ),
        Set[ShowingConstraintMock]()
      )
      val schedules: List[Schedule] = scheduleBuilder.getPossibleSchedules(scheduleConstraints)

      schedules should have size (1)
      schedules(0).showingIds should (contain (1009L) and contain (1007L))
    }
  }

  test("optimal scheduling - ???") {
    new Tiff2012Fixture {
      val scheduleConstraints: ScheduleConstraintsMock = new ScheduleConstraintsMock(
        Set[MovieConstraintMock](
          new MovieConstraintMock(156, 2),
          new MovieConstraintMock(159, 2)
        ),
        Set[ShowingConstraintMock]()
      )
      val schedules: List[Schedule] = scheduleBuilder.getPossibleSchedules(scheduleConstraints)

      schedules should have size (1)
//      schedules(0).showingIds should (contain (1009L) and contain (1007L))
    }
  }

  private def assertBasicScheduleInvariants(
    schedule: Schedule, scheduleConstraints: ScheduleConstraintsMock, festivalProgramme: FestivalProgramme) {
    assertNoOverlap(schedule, festivalProgramme)
    assertAllShowingsScheduled(schedule, scheduleConstraints, festivalProgramme)
    assertNoTrivialSchedulingOpportunitiesMissed(schedule, scheduleConstraints, festivalProgramme)
    assertMissedMoviesComputedCorrectly(schedule, scheduleConstraints, festivalProgramme)
  }

  /** Verifies that no scheduled showing overlaps another. */
  private def assertNoOverlap(schedule: Schedule, festivalProgramme: FestivalProgramme) {
    val intervals: RangeSet[DateTime] = TreeRangeSet.create[DateTime]()
    for (showingId <- schedule.showingIds) {
      val showing = festivalProgramme.getShowing(showingId)
      assert(!showing.overlapsWith(intervals), "Scheduled showing\n\t\t"+showing+"overlaps another scheduled showing.")
      intervals.add(showing.interval)
    }
  }

  /** Ensures that the showings pinned by the user are all transferred to the final schedule. */
  private def assertAllShowingsScheduled(
    schedule: Schedule, scheduleConstraints: ScheduleConstraintsMock, festivalProgramme: FestivalProgramme) {
    val showingIds: Set[Long] = schedule.showingIds
    for (showingConstraint <- scheduleConstraints.showingConstraints) {
      showingIds should contain (showingConstraint.showingId)
    }
  }

  /** Verifies that none of the missed movies could have been scheduled while keeping the currently scheduled showings
    * in place.
    */
  private def assertNoTrivialSchedulingOpportunitiesMissed(
    schedule: Schedule, scheduleConstraints: ScheduleConstraintsMock, festivalProgramme: FestivalProgramme) {
    val intervals: RangeSet[DateTime] = schedule.getShowingsIntervals(festivalProgramme)
    for (missedMovieId <- schedule.missedMovieIds) {
      for (movieShowing <- festivalProgramme.showingsOf(missedMovieId)) {
        assert(movieShowing.overlapsWith(intervals) || !movieShowing.within(scheduleConstraints.getTimeConstraints),
          "Unscheduled movie "+missedMovieId+" could have been scheduled in the slot:\n\t\t"+movieShowing)
      }
    }
  }

  def assertMissedMoviesComputedCorrectly(
    schedule: Schedule, scheduleConstraints: ScheduleConstraintsMock, festivalProgramme: FestivalProgramme) {
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
    schedule: Schedule, scheduleConstraints: ScheduleConstraintsMock, festivalProgramme: FestivalProgramme) {
    val movieIdConstraints = scheduleConstraints.movieConstraints.map(_.movieId) --
      schedule.showingIds.map(festivalProgramme.getShowing(_).movie.id) --
      schedule.missedMovieIds
    movieIdConstraints should be ('empty)
  }
}
