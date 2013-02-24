package org.ffplanner

import `def`.MovieDefinition

/**
 * Wrapper around [[org.ffplanner.def.MovieDefinition MovieDefinition]] that ensures proper definition of
 * `equals`/`hashCode`.
 *
 * @author Bogdan Dumitriu
 */
class Movie(val id: Long) extends MovieDefinition {

  def this(movieDefinition: MovieDefinition) = this(movieDefinition.getId)

  def getId: java.lang.Long = id

  def canEqual(other: Any): Boolean = other.isInstanceOf[Movie]

  override def equals(other: Any): Boolean = other match {
    case that: Movie => that.canEqual(this) && id == that.id
    case _ => false
  }

  override def hashCode: Int = id.hashCode
}
