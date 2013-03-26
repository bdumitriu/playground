package org.ffplanner

import `def`.ConstraintDefinition

/**
 * Wrapper around [[org.ffplanner.def.ConstraintDefinition ConstraintDefinition.Showing]] that ensures proper
 * definition of `equals`/`hashCode`.
 *
 * @author Bogdan Dumitriu
 */
class ShowingConstraint(val showingId: Long, val priority: Short) {

  def this(showingConstraint: ConstraintDefinition.Showing) =
    this(showingConstraint.getShowingId, showingConstraint.getPriority)

  def canEqual(other: Any): Boolean = other.isInstanceOf[ShowingConstraint]

  override def equals(other: Any): Boolean = other match {
    case that: ShowingConstraint => this.canEqual(that) && this.showingId == that.showingId
    case _ => false
  }

  override def hashCode: Int = showingId.hashCode
}
