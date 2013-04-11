package org.ffplanner

import org.ffplanner.`def`.{ConstraintDefinition, ScheduleConstraintsDefinition}
import com.google.common.collect.ImmutableRangeSet
import org.joda.time.DateTime
import java.util

/** Scala wrapper around [[org.ffplanner.def.ScheduleConstraintsDefinition ScheduleConstraintsDefinition]]. All
  * Java-specific definitions are transformed to Scala-specific ones.
  *
  * @author Bogdan Dumitriu
  */
class ScheduleConstraints(
    val movieConstraints: Set[MovieConstraint],
    val showingConstraints: Set[ShowingConstraint],
    val timeConstraints: ImmutableRangeSet[DateTime]) {

  def this(festivalProgramme: FestivalProgramme, constraints: ScheduleConstraintsDefinition) = this(
    ScheduleConstraints.movieConstraintsFrom(festivalProgramme, constraints),
    ScheduleConstraints.showingConstraintsFrom(festivalProgramme, constraints),
    Utils.safeTimeConstraints(constraints)
  )

  def this(movieConstraints: Set[MovieConstraint], showingConstraints: Set[ShowingConstraint]) =
    this(movieConstraints, showingConstraints, Utils.NoTimeConstraints)

  val movieConstraintIds: Set[Long] = movieConstraints map { _.movie.id }

  val showingConstraintIds: Set[Long] = showingConstraints map { _.showing.id }
}

private object ScheduleConstraints {

  def movieConstraintsFrom(
      festivalProgramme: FestivalProgramme,
      constraints: ScheduleConstraintsDefinition): Set[MovieConstraint] = {
    val safeMovieConstraints: util.Collection[ConstraintDefinition.Movie] =
      Utils.safeConstraints(constraints).getMovieConstraints
    Utils.ensureNonNull(safeMovieConstraints).map(new MovieConstraint(festivalProgramme, _)).toSet
  }

  def showingConstraintsFrom(
    festivalProgramme: FestivalProgramme,
    constraints: ScheduleConstraintsDefinition): Set[ShowingConstraint] = {
    val safeShowingConstraints: util.Collection[ConstraintDefinition.Showing] =
      Utils.safeConstraints(constraints).getShowingConstraints
    Utils.ensureNonNull(safeShowingConstraints).map(new ShowingConstraint(festivalProgramme, _)).toSet
  }
}
