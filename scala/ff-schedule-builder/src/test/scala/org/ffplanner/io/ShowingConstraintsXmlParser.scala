package org.ffplanner.io

import scala.xml.{Node, XML}
import org.ffplanner._
import org.ffplanner.`def`.ShowingDefinition
import org.joda.time.{DateTime, Period}

/** Parses an XML file with a schedule constraints definition to a
  * [[org.ffplanner.ScheduleConstraintsMock ScheduleConstraintsMock]].
  *
  * @author Bogdan Dumitriu
  */
class ShowingConstraintsXmlParser(val resourceName: String) {

  val constraintsNode = XML.load(classOf[ShowingConstraintsXmlParser].getResourceAsStream(resourceName))

  val movieConstraints: Set[MovieConstraintMock] =
    (constraintsNode \\ "movie").foldLeft(Set.empty[MovieConstraintMock])(addMovie)

  val showingConstraints: Set[ShowingConstraintMock] =
    (constraintsNode \\ "showing").foldLeft(Set.empty[ShowingConstraintMock])(addShowing)

  val scheduleConstraints = new ScheduleConstraintsMock(movieConstraints, showingConstraints)

  private def addMovie(movies: Set[MovieConstraintMock], movie: Node): Set[MovieConstraintMock] = {
    movies + parseMovie(movie)
  }

  private def parseMovie(movie: Node): MovieConstraintMock = {
    new MovieConstraintMock((movie \ "id").text.toLong, (movie \ "priority").text.toShort)
  }

  private def addShowing(showings: Set[ShowingConstraintMock], showing: Node): Set[ShowingConstraintMock] = {
    showings + parseShowing(showing)
  }

  private def parseShowing(showing: Node): ShowingConstraintMock = {
    new ShowingConstraintMock((showing \ "id").text.toLong, (showing \\ "priority").text.toShort)
  }
}
