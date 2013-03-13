package org.ffplanner

import `def`.ShowingDefinition
import org.joda.time.{Interval, Period, DateTime}
import java.lang
import com.google.common.collect.{RangeSet, Range, TreeRangeSet}

/**
 * Wrapper around [[org.ffplanner.def.ShowingDefinition ShowingDefinition]] that ensures proper definition of
 * `equals`/`hashCode`.
 *
 * @author Bogdan Dumitriu
 */
class Showing(val id: Long, val venueId: Long, val dateTime: DateTime, val movie: Movie) {

  val interval: Range[DateTime] = Range.closedOpen(this.dateTime, this.dateTime.plus(this.movie.duration))

  def this(showingDefinition: ShowingDefinition) = this(showingDefinition.getId, showingDefinition.getVenueId,
    showingDefinition.getDateTime, new Movie(showingDefinition.getMovie))

  def canEqual(other: Any): Boolean = other.isInstanceOf[Showing]

  override def equals(other: Any): Boolean = other match {
    case that: Showing =>
      that.canEqual(this) &&
        id == that.id && venueId == that.venueId && dateTime == that.dateTime && movie == that.movie
    case _ => false
  }

  override def hashCode(): Int =
    41 *
      (41 *
        (41 *
          (41 + id.hashCode) +
          venueId.hashCode) +
        dateTime.hashCode) +
      movie.hashCode
}
