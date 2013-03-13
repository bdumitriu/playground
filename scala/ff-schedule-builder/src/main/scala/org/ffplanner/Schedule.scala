package org.ffplanner

import collection.JavaConversions.seqAsJavaList

/**
 *
 *
 * @author Bogdan Dumitriu
 */
class Schedule(val showingIds: List[Long], val missedMovieIds: List[Long]) {

  val cachedShowingIdsJ: java.util.List[java.lang.Long] = showingIds.map {Long.box(_)}

  val cachedMissedMovieIdsJ: java.util.List[java.lang.Long] = missedMovieIds.map {Long.box(_)}

  def showingIdsJ: java.util.List[java.lang.Long] = cachedShowingIdsJ

  def missedMovieIdsJ: java.util.List[java.lang.Long] = cachedMissedMovieIdsJ
}
