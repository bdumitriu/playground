package org.ffplanner

import `def`.{ShowingDefinition, MovieDefinition, FestivalProgrammeDefinition}
import scala.collection.JavaConversions.collectionAsScalaIterable
import org.joda.time.DateTime

/**
 *
 *
 * @author Bogdan Dumitriu
 */
class FestivalProgramme(festivalProgrammeDefinition: FestivalProgrammeDefinition) {

  object DateTimeOrdering extends Ordering[DateTime] {
    def compare(x: DateTime, y: DateTime): Int = x.compareTo(y)
  }

  private val showingDefinitions: List[Showing] =
    Utils.ensureNonNull(festivalProgrammeDefinition.getShowings) map { new Showing(_) }

  private val movieShowings: Map[Movie, List[Showing]] =
    showingDefinitions.sortBy(_.getDateTime)(DateTimeOrdering) groupBy { _.getMovie }

  private val showings: Map[Long, Showing] = showingDefinitions.map(s => (Long2long(s.getId), s)).toMap

  def showingsOf(movieId: Long): List[Showing] = showingsOf(new Movie(movieId))

  def showingsOf(movie: Movie): List[Showing] = movieShowings(movie)

  def getShowing(showingId: Long): Showing = showings(showingId)

  def getMovieOfShowing(showingId: Long): Movie = getShowing(showingId).getMovie

  def getMovieIdOfShowing(showingId: Long): Long = getShowing(showingId).getMovie.id
}
