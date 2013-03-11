package org.ffplanner

import `def`.ConstraintDefinition

/**
 *
 *
 * @author Bogdan Dumitriu
 */
class TestShowingConstraint(val showingId: Long, val priority: Short) extends ConstraintDefinition.Showing {

  def getShowingId: java.lang.Long = showingId

  def getPriority: java.lang.Short = priority
}
