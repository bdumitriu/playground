package org.ffplanner

import org.ffplanner.`def`.{ConstraintDefinition, ScheduleConstraints}
import scala.collection.JavaConversions.asJavaCollection
import org.joda.time.DateTime
import com.google.common.collect.{Range, ImmutableRangeSet}

/**
  * @author Bogdan Dumitriu
  */
class ScheduleConstraintsMock(
    val movieConstraints: Set[MovieConstraintMock],
    val showingConstraints: Set[ShowingConstraintMock])
  extends ScheduleConstraints with ScheduleOperations {

  def getShowingConstraints: java.util.Collection[ConstraintDefinition.Showing] = showingConstraints

  def getMovieConstraints: java.util.Collection[ConstraintDefinition.Movie] = movieConstraints

  def getTimeConstraints: ImmutableRangeSet[DateTime] =
    ImmutableRangeSet.of(
      Range.closedOpen(new DateTime(java.lang.Long.MIN_VALUE), new DateTime(java.lang.Long.MAX_VALUE)))

  def getShowingsIntervals(festivalProgramme: FestivalProgramme) =
    getIntervalsOf(showingConstraints.map { showingConstraint => showingConstraint.showingId }, festivalProgramme)
}
