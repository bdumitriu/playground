package org.ffplanner

import `def`.MovieDefinition
import org.joda.time.{DateTimeConstants, Period}

/** Wrapper around [[org.ffplanner.def.MovieDefinition MovieDefinition]] that ensures proper definition of
  * `equals`/`hashCode`.
  *
  * @author Bogdan Dumitriu
  */
class Movie(val id: Long, val duration: Period) {

  def this(movieDefinition: MovieDefinition) = this(movieDefinition.getId, movieDefinition.getDuration)

  def this(id: Long) = this(id, Period.minutes(DateTimeConstants.MINUTES_PER_HOUR))

  def canEqual(other: Any): Boolean = other.isInstanceOf[Movie]

  override def equals(other: Any): Boolean = other match {
    case that: Movie => that.canEqual(this) && id == that.id
    case _ => false
  }

  override def hashCode: Int = id.hashCode

  override def toString: String = "M["+id+"]"
}
