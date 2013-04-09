package org.ffplanner

import `def`.{ScheduleConstraintsDefinition, ShowingDefinition}
import org.joda.time.DateTime
import com.google.common.collect.{ImmutableRangeSet, RangeSet, Range}

/** Wrapper around [[org.ffplanner.def.ShowingDefinition ShowingDefinition]] that ensures proper definition of
  * `equals`/`hashCode`.
  *
  * @author Bogdan Dumitriu
  */
class Showing(val id: Long, val venueId: Long, val dateTime: DateTime, val movie: Movie) {

  val interval: Range[DateTime] = Range.closedOpen(this.dateTime, this.dateTime.plus(this.movie.duration))

  def this(showingDefinition: ShowingDefinition) = this(showingDefinition.getId, showingDefinition.getVenueId,
    showingDefinition.getDateTime, new Movie(showingDefinition.getMovie))

  def within(timeConstraints: ImmutableRangeSet[DateTime]) = timeConstraints.encloses(interval)

  def overlapsWith(showing: Showing) =
    interval.isConnected(showing.interval) && !interval.intersection(showing.interval).isEmpty

  def overlapsWith(intervals: RangeSet[DateTime]) = !intervals.subRangeSet(interval).isEmpty

  def canEqual(other: Any): Boolean = other.isInstanceOf[Showing]

  override def equals(other: Any): Boolean = other match {
    case that: Showing => that.canEqual(this) && id == that.id
    case _ => false
  }

  override def hashCode: Int = id.hashCode

  override def toString: String = {
    "S"+id+" "+movie+"@ ["+toString(interval.lowerEndpoint)+"-"+toString(interval.upperEndpoint)+") @ "+venueId
  }

  private def toString(dateTime: DateTime) =
    dateTime.hourOfDay.get.formatted("%02d")+":"+dateTime.minuteOfHour.get.formatted("%02d")
}

object Showing {

  implicit object ShowingOrdering extends Ordering[Showing] {
    def compare(s1: Showing, s2: Showing): Int = {
      s1.dateTime compareTo s2.dateTime match {
        case result if result == 0 => s1.id compare s2.id
        case result => result
      }
    }
  }
}
