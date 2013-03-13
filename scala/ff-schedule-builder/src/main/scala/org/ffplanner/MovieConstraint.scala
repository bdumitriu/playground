package org.ffplanner

import `def`.ConstraintDefinition

/**
 * Wrapper around [[org.ffplanner.def.ConstraintDefinition ConstraintDefinition.Movie]] that ensures proper
 * definition of `equals`/`hashCode`.
 *
 * @author Bogdan Dumitriu
 */
class MovieConstraint(val movieId: Long, val priority: Short) {

  def this(movieConstraint: ConstraintDefinition.Movie) = this(movieConstraint.getMovieId, movieConstraint.getPriority)

  def canEqual(other: Any): Boolean = other.isInstanceOf[MovieConstraint]

  override def equals(other: Any): Boolean = other match {
    case that: MovieConstraint => this.canEqual(that) && this.movieId == that.movieId
    case _ => false
  }

  override def hashCode(): Int = movieId.hashCode
}
