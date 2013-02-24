package org.ffplanner

import `def`.ShowingDefinition
import org.joda.time.DateTime

/**
 * Wrapper around [[org.ffplanner.def.ShowingDefinition ShowingDefinition]] that ensures proper definition of
 * `equals`/`hashCode`.
 *
 * @author Bogdan Dumitriu
 */
class Showing(val id: Long, val dateTime: DateTime, val movie: Movie) extends ShowingDefinition {

  def this(showingDefinition: ShowingDefinition) =
    this(showingDefinition.getId, showingDefinition.getDateTime, new Movie(showingDefinition.getMovie))

  def getId: java.lang.Long = id

  def getDateTime: DateTime = dateTime

  def getMovie: Movie = movie

  def canEqual(other: Any): Boolean = other.isInstanceOf[Showing]

  override def equals(other: Any): Boolean = other match {
    case that: Showing => that.canEqual(this) && id == that.id && dateTime == that.dateTime && movie == that.movie
    case _ => false
  }

  override def hashCode(): Int = 41 * (41 * (41 + id.hashCode) + dateTime.hashCode) + movie.hashCode
}
