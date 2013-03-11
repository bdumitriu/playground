package org.ffplanner

import org.ffplanner.`def`.{ConstraintDefinition, ScheduleDefinition}
import scala.collection.JavaConversions.asJavaCollection

/**
 *
 *
 * @author Bogdan Dumitriu
 */
class TestScheduleDefinition(
    val movies: List[ConstraintDefinition.Movie],
    val showings: List[ConstraintDefinition.Showing])
  extends ScheduleDefinition {

  def getShowingConstraints: java.util.Collection[ConstraintDefinition.Showing] = showings

  def getMovieConstraints: java.util.Collection[ConstraintDefinition.Movie] = movies
}
