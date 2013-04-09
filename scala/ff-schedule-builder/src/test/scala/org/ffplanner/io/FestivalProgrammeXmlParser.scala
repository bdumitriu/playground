package org.ffplanner.io

import scala.xml.{Node, XML}
import org.joda.time.{DateTime, Period}
import org.ffplanner.{ShowingMock, MovieMock}

/** Parses an XML file with a festival programme definition to a list of [[org.ffplanner.ShowingMock ShowingMock]]s.
  *
  * @author Bogdan Dumitriu
  */
class FestivalProgrammeXmlParser(val resourceName: String) {

  val programmeNode = XML.load(classOf[FestivalProgrammeXmlParser].getResourceAsStream(resourceName))

  val movieMap: Map[Long, MovieMock] =
    (programmeNode \\ "movie").reverse.foldLeft(List.empty[MovieMock])(addMovie).map(m => (m.id, m)).toMap

  val showings: List[ShowingMock] =
    (programmeNode \\ "showing").reverse.foldLeft(List.empty[ShowingMock])(addShowing)

  private def addMovie(movies: List[MovieMock], movie: Node): List[MovieMock] = {
    parseMovie(movie) :: movies
  }

  private def parseMovie(movie: Node): MovieMock = {
    new MovieMock((movie \ "id").text.toLong, Period.minutes((movie \ "duration").text.toInt))
  }

  private def addShowing(showings: List[ShowingMock], showing: Node): List[ShowingMock] = {
    parseShowing(showing) :: showings
  }

  private def parseShowing(showing: Node): ShowingMock = {
    new ShowingMock(
      (showing \ "id").text.toLong,
      (showing \ "venue-id").text.toLong,
      DateTime.parse((showing \ "datetime").text),
      movieMap((showing \ "movie-id").text.toLong))
  }
}
