package org.ffplanner

/** Creates a schedule starting from the input `constraints`.
  *
  * @author Bogdan Dumitriu
  */
class ScheduleCreator(val scheduleBuilder: ScheduleBuilder, val constraints: ScheduleConstraints)
  extends ScheduleOperations {

  val festivalProgramme: FestivalProgramme = scheduleBuilder.festivalProgramme

  /**
    * @return one or more proposed schedules.
    */
  def getSchedules: List[Schedule] = {
    class ConflictSplit(val nonConflictShowingIds: Set[Long], val conflictMovieIds: Set[Long])

    /** Divides the `movieIdsToSchedule` into no conflicts (for which the first non conflictual showing id is returned) /
      * with conflicts (for which the movieId is returned).
      *
      * @param scheduledShowingIds the showings that are already scheduled
      * @param movieIdsToSchedule the movies to split
      * @return the set of non conflictual showing ids and the set of movies for which no showing is conflict-free.
      */
    def splitMovieIds(scheduledShowingIds: Set[Long], movieIdsToSchedule: Set[Long]): ConflictSplit = {

      def splitAccordingToConflict(acc: ConflictSplit, movieIdToSchedule: Long): ConflictSplit = {
        getEarliestShowingIdWithNoConflict(movieIdToSchedule, movieIdsToSchedule, scheduledShowingIds) match {
          case Some(showingId) => new ConflictSplit(acc.nonConflictShowingIds + showingId, acc.conflictMovieIds)
          case None => new ConflictSplit(acc.nonConflictShowingIds, acc.conflictMovieIds + movieIdToSchedule)
        }
      }

      movieIdsToSchedule.foldLeft(new ConflictSplit(Set(), Set()))(splitAccordingToConflict)
    }

    def getEarliestShowingIdWithNoConflict(movieId: Long, movieIds: Set[Long], showingIds: Set[Long]): Option[Long] = {
      festivalProgramme.showingsOf(movieId).
        find(s => festivalProgramme.getConflictsOf(s.id, movieIds, showingIds).isEmpty).map(_.id)
    }

    def getSchedules0(showingIds: Set[Long], movieIds: Set[Long]): ConflictSplit = {
      val conflictSplit: ConflictSplit = splitMovieIds(showingIds, movieIds)
      if (conflictSplit.nonConflictShowingIds.isEmpty) {
        new ConflictSplit(showingIds, conflictSplit.conflictMovieIds)
      } else {
        getSchedules0(showingIds ++ conflictSplit.nonConflictShowingIds, conflictSplit.conflictMovieIds)
      }
    }

    val conflictSplit: ConflictSplit = getSchedules0(constraints.showingConstraintIds, constraints.movieConstraintIds)
    List(new Schedule(conflictSplit.nonConflictShowingIds, conflictSplit.conflictMovieIds))
  }

  /**
   * @return a range set covering the intervals of all the showings imposed via the `showingsConstraints`.
   */
  def getShowingsIntervals = getIntervalsOf(constraints.showingConstraintIds, festivalProgramme)
}
