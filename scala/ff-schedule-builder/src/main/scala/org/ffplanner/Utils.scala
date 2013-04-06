package org.ffplanner

import scala.collection.JavaConversions.collectionAsScalaIterable

/**
  * @author Bogdan Dumitriu
  */
object Utils {

  def ensureNonNull[T](collection: java.util.Collection[T]): List[T] =
    Option(collection).getOrElse(java.util.Collections.emptyList()).toList
}
