package org.ffplanner

import org.ffplanner.`def`.{ConstraintDefinition, ScheduleConstraints}
import scala.collection.JavaConversions.asJavaCollection
import java.util
import org.joda.time.Interval

/**
 *
 *
 * @author Bogdan Dumitriu
 */
class TestScheduleConstraints(
    val movies: List[ConstraintDefinition.Movie],
    val showings: List[ConstraintDefinition.Showing])
  extends ScheduleConstraints {

  def getShowingConstraints: java.util.Collection[ConstraintDefinition.Showing] = showings

  def getMovieConstraints: java.util.Collection[ConstraintDefinition.Movie] = movies

  def getTimeConstraints: util.Collection[Interval] = ???
}
