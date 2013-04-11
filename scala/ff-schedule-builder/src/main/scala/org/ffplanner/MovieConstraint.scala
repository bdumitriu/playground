package org.ffplanner

import `def`.ConstraintDefinition

/** Wrapper around [[org.ffplanner.def.ConstraintDefinition ConstraintDefinition.Movie]] that ensures proper
  * definition of `equals`/`hashCode`.
  *
  * @author Bogdan Dumitriu
  */
class MovieConstraint(val movie: Movie, val priority: Short) {

  def this(festivalProgramme: FestivalProgramme, movieConstraint: ConstraintDefinition.Movie) =
    this(festivalProgramme.getMovie(movieConstraint.getMovieId), movieConstraint.getPriority)

  def canEqual(other: Any): Boolean = other.isInstanceOf[MovieConstraint]

  override def equals(other: Any): Boolean = other match {
    case that: MovieConstraint => this.canEqual(that) && this.movie == that.movie
    case _ => false
  }

  override def hashCode: Int = movie.hashCode
}
