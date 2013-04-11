package org.ffplanner

import `def`.ConstraintDefinition

/** Wrapper around [[org.ffplanner.def.ConstraintDefinition ConstraintDefinition.Showing]] that ensures proper
  * definition of `equals`/`hashCode`.
  *
  * @author Bogdan Dumitriu
  */
class ShowingConstraint(val showing: Showing, val priority: Short) {

  def this(festivalProgramme: FestivalProgramme, showingConstraint: ConstraintDefinition.Showing) =
    this(festivalProgramme.getShowing(showingConstraint.getShowingId), showingConstraint.getPriority)

  def canEqual(other: Any): Boolean = other.isInstanceOf[ShowingConstraint]

  override def equals(other: Any): Boolean = other match {
    case that: ShowingConstraint => this.canEqual(that) && this.showing == that.showing
    case _ => false
  }

  override def hashCode: Int = showing.hashCode
}
