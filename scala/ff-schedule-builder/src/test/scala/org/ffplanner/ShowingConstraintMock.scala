package org.ffplanner

import `def`.ConstraintDefinition

/**
  * @author Bogdan Dumitriu
  */
class ShowingConstraintMock(val showingId: Long, val priority: Short) extends ConstraintDefinition.Showing {

  def getShowingId: java.lang.Long = showingId

  def getPriority: java.lang.Short = priority

  def canEqual(other: Any): Boolean = other.isInstanceOf[ShowingConstraintMock]

  override def equals(other: Any): Boolean = other match {
    case that: ShowingConstraintMock => this.canEqual(that) && this.showingId == that.showingId
    case _ => false
  }

  override def hashCode: Int = showingId.hashCode
}
