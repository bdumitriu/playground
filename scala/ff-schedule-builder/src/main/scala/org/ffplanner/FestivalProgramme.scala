package org.ffplanner

import `def`.{ShowingDefinition, FestivalProgrammeDefinition}
import scala.collection.immutable.SortedSet
import org.ffplanner.Showing.ShowingOrdering

/** Stores the movies and showings that define the programme of a festival.
  *
  * @author Bogdan Dumitriu
  */
class FestivalProgramme(festivalProgrammeDefinition: FestivalProgrammeDefinition) {

  private val sortedShowings: SortedSet[Showing] =
    SortedSet(wrap(festivalProgrammeDefinition.getShowings): _*)

  private val movieShowings: Map[Movie, SortedSet[Showing]] =
    sortedShowings groupBy { _.movie } withDefaultValue SortedSet[Showing]()

  private val movies: Map[Long, Movie] = movieShowings.keySet.map(m => (m.id, m)).toMap

  private val showings: Map[Long, Showing] = sortedShowings.map(s => (s.id, s)).toMap

  private val conflictMap: ConflictMap = new ConflictMap(this)

  private def wrap(showings: java.util.List[ShowingDefinition]): List[Showing] =
    Utils.ensureNonNull(showings).map(new Showing(_))

  def getSortedShowings = sortedShowings

  def getMovies = movieShowings.keySet

  def showingsOf(movieId: Long): SortedSet[Showing] = showingsOf(new Movie(movieId))

  def showingsOf(movie: Movie): SortedSet[Showing] = movieShowings(movie)

  def getMovie(movieId: Long): Movie = movies(movieId)

  def getShowing(showingId: Long): Showing = showings(showingId)

  def getMovieOfShowing(showingId: Long): Movie = getShowing(showingId).movie

  def getMovieIdOfShowing(showingId: Long): Long = getShowing(showingId).movie.id

  /**
    * @param showingId indicates the showing to test for conflicts
    * @param movieIds the movies to include in the conflict scope
    * @param showingIds the showings to include in the conflict scope
    * @return any showings of `movieIds` or from among `showingIds` that conflict with `showingId`. `showingId` will
    *         never be in conflict with itself or other showings of the movie being shown as `showingId`.
    */
  def getConflictsOf(showingId: Long, movieIds: Set[Long], showingIds: Set[Long]): Set[Long] =
    conflictMap.conflictingShowingIds(showingId, movieIds, showingIds)
}
