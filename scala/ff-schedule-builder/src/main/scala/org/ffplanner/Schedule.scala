package org.ffplanner

import collection.JavaConversions.setAsJavaSet

/**
  * @author Bogdan Dumitriu
  */
class Schedule(val showingIds: Set[Long], val missedMovieIds: Set[Long]) extends ScheduleOperations {

  val cachedShowingIdsJ: java.util.Set[java.lang.Long] = showingIds.map(Long.box)

  val cachedMissedMovieIdsJ: java.util.Set[java.lang.Long] = missedMovieIds.map(Long.box)

  def showingIdsJ: java.util.Set[java.lang.Long] = cachedShowingIdsJ

  def missedMovieIdsJ: java.util.Set[java.lang.Long] = cachedMissedMovieIdsJ

  def getShowingsIntervals(festivalProgramme: FestivalProgramme) = getIntervalsOf(showingIds, festivalProgramme)
}
