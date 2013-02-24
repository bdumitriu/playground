package org.ffplanner

import collection.JavaConversions.seqAsJavaList

/**
 *
 *
 * @author Bogdan Dumitriu
 */
class Schedule(val showings: List[Long], val missedMovies: List[Long]) {

  val cachedShowingsJ: java.util.List[java.lang.Long] = showings.map {Long.box(_)}

  val cachedMissedMoviesJ: java.util.List[java.lang.Long] = missedMovies.map {Long.box(_)}

  def showingsJ: java.util.List[java.lang.Long] = cachedShowingsJ

  def missedMoviesJ: java.util.List[java.lang.Long] = cachedMissedMoviesJ
}
