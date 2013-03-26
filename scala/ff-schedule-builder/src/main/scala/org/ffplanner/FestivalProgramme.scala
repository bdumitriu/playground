package org.ffplanner

import `def`.{ShowingDefinition, FestivalProgrammeDefinition}
import org.joda.time.DateTime
import collection.SortedSet

/**
 *
 *
 * @author Bogdan Dumitriu
 */
class FestivalProgramme(festivalProgrammeDefinition: FestivalProgrammeDefinition) {

  private val showingDefinitions: SortedSet[Showing] =
    SortedSet(wrap(festivalProgrammeDefinition.getShowings): _*)(Ordering.fromLessThan(_.dateTime isBefore _.dateTime))

  private val movieShowings: Map[Movie, SortedSet[Showing]] = showingDefinitions groupBy { _.movie }

  private val showings: Map[Long, Showing] = showingDefinitions.map(s => (Long2long(s.id), s)).toMap

  private def wrap(showings: java.util.List[ShowingDefinition]): List[Showing] =
    Utils.ensureNonNull(showings).map(new Showing(_))

  def showingsOf(movieId: Long): SortedSet[Showing] = showingsOf(new Movie(movieId))

  def showingsOf(movie: Movie): SortedSet[Showing] = movieShowings(movie)

  def getShowing(showingId: Long): Showing = showings(showingId)

  def getMovieOfShowing(showingId: Long): Movie = getShowing(showingId).movie

  def getMovieIdOfShowing(showingId: Long): Long = getShowing(showingId).movie.id
}
