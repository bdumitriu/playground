package org.ffplanner

import `def`.MovieDefinition
import org.joda.time.{DateTimeConstants, Period}
import java.lang

/**
 *
 *
 * @author Bogdan Dumitriu
 */
class TestMovie(val id: Long, val duration: Period) extends MovieDefinition {

  def getId: lang.Long = id

  def getDuration: Period = duration

  def canEqual(other: Any): Boolean = other.isInstanceOf[TestMovie]

  override def equals(other: Any): Boolean = other match {
    case that: TestMovie => this.canEqual(that) && this.id == that.id
    case _ => false
  }

  override def hashCode: Int = id.hashCode
}
