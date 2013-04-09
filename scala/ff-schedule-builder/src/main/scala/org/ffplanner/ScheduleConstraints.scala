package org.ffplanner

import org.ffplanner.`def`.ScheduleConstraintsDefinition
import com.google.common.collect.{Range, ImmutableRangeSet}
import org.joda.time.DateTime

/** Scala wrapper around [[org.ffplanner.def.ScheduleConstraintsDefinition ScheduleConstraintsDefinition]]. All
  * Java-specific definitions are transformed to Scala-specific ones.
  *
  * @author Bogdan Dumitriu
  */
class ScheduleConstraints(
    val movieConstraints: Set[MovieConstraint],
    val showingConstraints: Set[ShowingConstraint],
    val timeConstraints: ImmutableRangeSet[DateTime]) {

  def this(constraints: ScheduleConstraintsDefinition) = this(
    Utils.ensureNonNull(Utils.safeConstraints(constraints).getMovieConstraints).map(new MovieConstraint(_)).toSet,
    Utils.ensureNonNull(Utils.safeConstraints(constraints).getShowingConstraints).map(new ShowingConstraint(_)).toSet,
    Utils.safeTimeConstraints(constraints)
  )

  def this(movieConstraints: Set[MovieConstraint], showingConstraints: Set[ShowingConstraint]) =
    this(movieConstraints, showingConstraints, Utils.noTimeConstraints)

  val movieConstraintIds: Set[Long] = movieConstraints map { s => Long2long(s.movieId) }

  val showingConstraintIds: Set[Long] = showingConstraints map { s => Long2long(s.showingId) }


}
