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
}
