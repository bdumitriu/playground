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

  def canEqual(other: Any): Boolean = other.isInstanceOf[TestMovieConstraint]

  override def equals(other: Any): Boolean = other match {
    case that: TestMovieConstraint => this.canEqual(that) && this.movieId == that.movieId
    case _ => false
  }

  override def hashCode(): Int = movieId.hashCode
}
