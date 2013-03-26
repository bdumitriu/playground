package org.ffplanner

import com.google.common.collect.TreeRangeSet
import org.joda.time.DateTime

/**
 *
 *
 * @author Bogdan Dumitriu
 */
trait ScheduleOperations {

  /**
   * @return a range set covering the intervals of all the showings in `showingIds`.
   */
  protected def getIntervalsOf(showingIds: Set[Long], festivalProgramme: FestivalProgramme): TreeRangeSet[DateTime] = {
    val rangeSet: TreeRangeSet[DateTime] = TreeRangeSet.create[DateTime]()
    showingIds.foreach { showingId => rangeSet.add(festivalProgramme.getShowing(showingId).interval) }
    rangeSet
  }
}
