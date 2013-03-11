package org.ffplanner

import org.ffplanner.`def`.ConstraintDefinition

/**
 *
 *
 * @author Bogdan Dumitriu
 */
class TestMovieConstraint(val movieId: Long, val priority: Short) extends ConstraintDefinition.Movie {

  def getMovieId: java.lang.Long = movieId

  def getPriority: java.lang.Short = priority
}
