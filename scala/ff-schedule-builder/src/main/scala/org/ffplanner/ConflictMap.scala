package org.ffplanner

import collection.immutable.SortedSet

/** A conflict map storing which showings a given showing in a [[org.ffplanner.FestivalProgramme FestivalProgramme]] is
  * in conflict with.
  *
  *  @author Bogdan Dumitriu
  */
class ConflictMap(val festivalProgramme: FestivalProgramme) {

  private val conflictMap: Map[Long, SortedSet[Showing]] = buildConflictMap

  private def buildConflictMap: Map[Long, SortedSet[Showing]] = {
    class Acc(
        val showings: SortedSet[Showing],
        val slidingWindow: SortedSet[Showing],
        val conflictMap: Map[Long, SortedSet[Showing]]) {}

    def f(acc: Acc, showing: Showing): Acc = {
      val showingsSplit: (SortedSet[Showing], SortedSet[Showing]) = acc.showings.span(_.overlapsWith(showing))
      val slidingWindowSplit: (SortedSet[Showing], SortedSet[Showing]) =
        acc.slidingWindow.partition(_.overlapsWith(showing))
      val newShowings: SortedSet[Showing] = showingsSplit._2 ++ slidingWindowSplit._2.filter(_.after(showing))
      val newSlidingWindow: SortedSet[Showing] = showingsSplit._1 ++ slidingWindowSplit._1
      new Acc(newShowings, newSlidingWindow,
        acc.conflictMap + (showing.id -> (newSlidingWindow -- festivalProgramme.showingsOf(showing.movie))))
    }

    val sortedShowings: SortedSet[Showing] = festivalProgramme.getSortedShowings
    sortedShowings.foldLeft(new Acc(sortedShowings, SortedSet()(sortedShowings.ordering), Map()))(f).conflictMap
  }

  /**
    * @param showingId indicates the showing to test for conflicts
    * @param movieIds the movies to include in the conflict scope
    * @param showingIds the showings to include in the conflict scope
    * @return any showings of `movieIds` or from among `showingIds` that conflict with `showingId`. `showingId` will
    *         never be in conflict with itself or other showings of the movie being shown as `showingId`.
    */
  def conflictingShowingIds(showingId: Long, movieIds: Set[Long], showingIds: Set[Long]): Set[Long] =
    conflictMap(showingId).filter(s => movieIds.contains(s.movie.id) || showingIds.contains(s.id)).map(_.id)
}
