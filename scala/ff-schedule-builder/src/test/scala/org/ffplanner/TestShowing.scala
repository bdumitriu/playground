package org.ffplanner

import `def`.{MovieDefinition, ShowingDefinition}
import org.joda.time.DateTime
import com.google.common.collect.Range
import java.lang

/**
 *
 *
 * @author Bogdan Dumitriu
 */
class TestShowing(val id: Long, val venueId: Long, val dateTime: DateTime, val movieDefinition: MovieDefinition)
  extends ShowingDefinition {

  def getId: lang.Long = id

  def getVenueId: lang.Long = venueId

  def getDateTime: DateTime = dateTime

  def getMovie: MovieDefinition = movieDefinition
}
