package org.ffplanner

import `def`.MovieDefinition
import org.joda.time.Period
import java.lang

/**
  * @author Bogdan Dumitriu
  */
class MovieMock(val id: Long, val duration: Period) extends MovieDefinition {

  def getId: lang.Long = id

  def getDuration: Period = duration

  def canEqual(other: Any): Boolean = other.isInstanceOf[MovieMock]

  override def equals(other: Any): Boolean = other match {
    case that: MovieMock => this.canEqual(that) && this.id == that.id
    case _ => false
  }

  override def hashCode: Int = id.hashCode
}
