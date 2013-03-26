package org.ffplanner

import `def`.ScheduleConstraints
import com.google.common.collect.TreeRangeSet
import org.joda.time.DateTime
import collection.SortedSet

/**
 *
 *
 * @author Bogdan Dumitriu
 */
class ScheduleCreator(val scheduleBuilder: ScheduleBuilder, private val constraints: ScheduleConstraints)
  extends ScheduleOperations {

  val festivalProgramme: FestivalProgramme = scheduleBuilder.festivalProgramme

  val scheduleConstraints: ScheduleConstraints = Option(constraints).getOrElse(ScheduleConstraints.EMPTY)

  val movieConstraints: Set[MovieConstraint] =
    Utils.ensureNonNull(scheduleConstraints.getMovieConstraints).map(new MovieConstraint(_)).toSet

  val showingConstraints: Set[ShowingConstraint] =
    Utils.ensureNonNull(scheduleConstraints.getShowingConstraints).map(new ShowingConstraint(_)).toSet

  val movieConstraintIds: Set[Long] = movieConstraints map { s => Long2long(s.movieId) }

  val showingConstraintIds: Set[Long] = showingConstraints map { s => Long2long(s.showingId) }

  def getSchedules: List[Schedule] = {
    def addShowing(acc: (Set[Long], TreeRangeSet[DateTime]), movieConstraintId: Long) = {
      val movieShowings: SortedSet[Showing] = festivalProgramme.showingsOf(movieConstraintId)
      movieShowings.find(showing => acc._2.subRangeSet(showing.interval).isEmpty) match {
        case Some(showing) => { acc._2.add(showing.interval); (acc._1 + showing.id, acc._2) }
        case None => (acc._1, acc._2)
      }
    }

    val showingIds: Set[Long] =
      movieConstraintIds.foldLeft((showingConstraintIds, getShowingsIntervals))(addShowing)._1
    List(new Schedule(showingIds, movieConstraintIds -- showingIds.map(festivalProgramme.getShowing(_).movie.id)))
  }

  /**
   * @return a range set covering the intervals of all the showings imposed via the `showingsConstraints`.
   */
  def getShowingsIntervals = getIntervalsOf(showingConstraintIds, festivalProgramme)
}
