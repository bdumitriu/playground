package org.ffplanner

import `def`.{ConstraintDefinition, ScheduleConstraints}
import com.google.common.collect.{TreeRangeSet, RangeSet}
import org.joda.time.DateTime

/**
 *
 *
 * @author Bogdan Dumitriu
 */
class ScheduleCreator(val scheduleBuilder: ScheduleBuilder, private val constraints: ScheduleConstraints) {

  val scheduleConstraints: ScheduleConstraints =
    Option(constraints).getOrElse(ScheduleConstraints.EMPTY)

  val movieConstraints: List[MovieConstraint] =
    Utils.ensureNonNull(scheduleConstraints.getMovieConstraints).map(new MovieConstraint(_)).distinct

  val showingConstraints: List[ShowingConstraint] =
    Utils.ensureNonNull(scheduleConstraints.getShowingConstraints).map(new ShowingConstraint(_)).distinct

  val movieConstraintIds: List[Long] = movieConstraints map { s => Long2long(s.movieId) }

  val showingConstraintIds: List[Long] = showingConstraints map { s => Long2long(s.showingId) }

  private val rangeSet: RangeSet[DateTime] = TreeRangeSet.create[DateTime]()

  def getSchedules: List[Schedule] = {
    val showingIds: List[Long] =
      movieConstraints.foldLeft((List[Long](), TreeRangeSet.create[DateTime]()))(addShowing)._1
    List(new Schedule(showingIds, List.empty))
  }

  private def addShowing(acc: (List[Long], TreeRangeSet[DateTime]), movieConstraint: MovieConstraint) = {
    val showings: List[Showing] = scheduleBuilder.festivalProgramme.showingsOf(movieConstraint.movieId)
    showings.find(showing => acc._2.subRangeSet(showing.interval).isEmpty) match {
      case Some(showing) => { acc._2.add(showing.interval); (showing.id :: acc._1, acc._2) }
      case None => (acc._1, acc._2)
    }
  }
}
