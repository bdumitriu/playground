package org.ffplanner

import scala.collection.JavaConversions.collectionAsScalaIterable
import org.ffplanner.`def`.ScheduleConstraintsDefinition
import org.joda.time.DateTime
import com.google.common.collect.{Range, ImmutableRangeSet}

/**
  * @author Bogdan Dumitriu
  */
object Utils {

  val NoTimeConstraints =
    ImmutableRangeSet.of(Range.closedOpen(new DateTime(Long.MinValue), new DateTime(Long.MaxValue)))

  val DefaultPriority: Short = 2

  def ensureNonNull[T](collection: java.util.Collection[T]): List[T] = {
    Option(collection).getOrElse(java.util.Collections.emptyList()).toList
  }

  def safeConstraints(constraints: ScheduleConstraintsDefinition) = {
    Option(constraints).getOrElse(ScheduleConstraintsDefinition.EMPTY)
  }

  def safeTimeConstraints(constraints: ScheduleConstraintsDefinition) = {
    Option(safeConstraints(constraints).getTimeConstraints).getOrElse(NoTimeConstraints)
  }
}
