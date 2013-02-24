package org.ffplanner

import `def`.{ShowingDefinition, MovieDefinition, FestivalProgrammeDefinition}
import scala.collection.JavaConversions.collectionAsScalaIterable
import org.joda.time.DateTime

/**
 *
 *
 * @author Bogdan Dumitriu
 */
class FestivalProgramme(val festivalProgrammeDefinition: FestivalProgrammeDefinition) {

  object DateTimeOrdering extends Ordering[DateTime] {
    def compare(x: DateTime, y: DateTime): Int = x.compareTo(y)
  }

  private val movieShowings: Map[Movie, List[Showing]] =
    festivalProgrammeDefinition.getShowings.toList.sortBy(_.getDateTime)(DateTimeOrdering).map(new Showing(_)).groupBy(_.getMovie)

  private val showings: Map[Long, Showing] =
    festivalProgrammeDefinition.getShowings.map(s => (Long2long(s.getId), new Showing(s))).toMap

  def showingsOf(movieId: Long): List[Showing] = showingsOf(new Movie(movieId))

  def showingsOf(movie: Movie): List[Showing] = movieShowings(movie)

  def getShowing(showingId: Long): Showing = showings(showingId)

  def getMovie(showingId: Long): Movie = getShowing(showingId).getMovie

  def getMovieId(showingId: Long): Long = getShowing(showingId).getMovie.id
}
