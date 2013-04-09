package org.ffplanner

import `def`.{ShowingDefinition, FestivalProgrammeDefinition}
import org.joda.time.{Period, DateTime}
import java.util
import org.ffplanner.io.FestivalProgrammeXmlParser
import scala.collection.JavaConversions.seqAsJavaList

/** Festival programmes that can be used in tests.
  *
  * @author Bogdan Dumitriu
  */
trait TestFestivalProgrammes {

  /** 1 movie shown twice in a row at the same venue */
  val festivalProgrammeDefinition1 = new FestivalProgrammeDefinition {

    def getShowings: java.util.List[ShowingDefinition] = {
      val showings: java.util.LinkedList[ShowingDefinition] = new java.util.LinkedList[ShowingDefinition]()
      val movie: MovieMock = new MovieMock(10, Period.minutes(60))
      showings.add(new ShowingMock(15, 100, DateTime.parse("2012-06-01T10:30"), movie))
      showings.add(new ShowingMock(25, 100, DateTime.parse("2012-06-01T11:30"), movie))
      showings
    }
  }

  val festivalProgramme1 = new FestivalProgramme(festivalProgrammeDefinition1)

  /** 12 movies shown at 2 venues the same day. Each movie is shown once. */
  val festivalProgrammeDefinition2 = new FestivalProgrammeDefinition {

    def getShowings: java.util.List[ShowingDefinition] = {
      val movies: Map[Long, MovieMock] = List(
        new MovieMock(1, Period.minutes(76)),
        new MovieMock(2, Period.minutes(104)),
        new MovieMock(3, Period.minutes(99)),
        new MovieMock(4, Period.minutes(121)),
        new MovieMock(5, Period.minutes(93)),
        new MovieMock(6, Period.minutes(76)),
        new MovieMock(7, Period.minutes(135)),
        new MovieMock(8, Period.minutes(105)),
        new MovieMock(9, Period.minutes(84)),
        new MovieMock(10, Period.minutes(97)),
        new MovieMock(11, Period.minutes(116)),
        new MovieMock(12, Period.minutes(88))
      ).map(m => (m.id, m)).toMap

      val showings: java.util.LinkedList[ShowingDefinition] = new java.util.LinkedList[ShowingDefinition]()
      showings.add(new ShowingMock(1001, 100, DateTime.parse("2012-06-01T11:00"), movies(1)))
      showings.add(new ShowingMock(1002, 100, DateTime.parse("2012-06-01T13:00"), movies(2)))
      showings.add(new ShowingMock(1003, 100, DateTime.parse("2012-06-01T15:30"), movies(3)))
      showings.add(new ShowingMock(1004, 100, DateTime.parse("2012-06-01T18:00"), movies(4)))
      showings.add(new ShowingMock(1005, 100, DateTime.parse("2012-06-01T21:00"), movies(5)))
      showings.add(new ShowingMock(1006, 100, DateTime.parse("2012-06-01T23:00"), movies(6)))
      showings.add(new ShowingMock(1007, 101, DateTime.parse("2012-06-01T10:00"), movies(7)))
      showings.add(new ShowingMock(1008, 101, DateTime.parse("2012-06-01T12:30"), movies(8)))
      showings.add(new ShowingMock(1009, 101, DateTime.parse("2012-06-01T15:00"), movies(9)))
      showings.add(new ShowingMock(1010, 101, DateTime.parse("2012-06-01T17:30"), movies(10)))
      showings.add(new ShowingMock(1011, 101, DateTime.parse("2012-06-01T20:00"), movies(11)))
      showings.add(new ShowingMock(1012, 101, DateTime.parse("2012-06-01T22:30"), movies(12)))
      showings
    }
  }

  val festivalProgramme2 = new FestivalProgramme(festivalProgrammeDefinition2)

  /** 9 movies shown at 2 venues the same day. Movies 1, 3 and 4 are shown twice (once in venue 1, once in venue 2),
    * the others only once.
    */
  val festivalProgrammeDefinition3 = new FestivalProgrammeDefinition {

    def getShowings: java.util.List[ShowingDefinition] = {
      val movies: Map[Long, MovieMock] = List(
        new MovieMock(1, Period.minutes(76)),
        new MovieMock(2, Period.minutes(104)),
        new MovieMock(3, Period.minutes(99)),
        new MovieMock(4, Period.minutes(121)),
        new MovieMock(5, Period.minutes(93)),
        new MovieMock(6, Period.minutes(76)),
        new MovieMock(7, Period.minutes(135)),
        new MovieMock(8, Period.minutes(105)),
        new MovieMock(10, Period.minutes(97))
      ).map(m => (m.id, m)).toMap

      val showings: java.util.LinkedList[ShowingDefinition] = new java.util.LinkedList[ShowingDefinition]()
      showings.add(new ShowingMock(1001, 100, DateTime.parse("2012-06-01T11:00"), movies(1)))
      showings.add(new ShowingMock(1002, 100, DateTime.parse("2012-06-01T13:00"), movies(2)))
      showings.add(new ShowingMock(1003, 100, DateTime.parse("2012-06-01T15:30"), movies(3)))
      showings.add(new ShowingMock(1004, 100, DateTime.parse("2012-06-01T18:00"), movies(4)))
      showings.add(new ShowingMock(1005, 100, DateTime.parse("2012-06-01T21:00"), movies(5)))
      showings.add(new ShowingMock(1006, 100, DateTime.parse("2012-06-01T23:00"), movies(6)))
      showings.add(new ShowingMock(1007, 101, DateTime.parse("2012-06-01T10:00"), movies(7)))
      showings.add(new ShowingMock(1008, 101, DateTime.parse("2012-06-01T12:30"), movies(8)))
      showings.add(new ShowingMock(1009, 101, DateTime.parse("2012-06-01T15:00"), movies(1)))
      showings.add(new ShowingMock(1010, 101, DateTime.parse("2012-06-01T17:30"), movies(10)))
      showings.add(new ShowingMock(1011, 101, DateTime.parse("2012-06-01T20:00"), movies(3)))
      showings.add(new ShowingMock(1012, 101, DateTime.parse("2012-06-01T22:30"), movies(4)))
      showings
    }
  }

  val festivalProgramme3 = new FestivalProgramme(festivalProgrammeDefinition3)

  /** 1 movie shown at the overlapping times at two different venues */
  val festivalProgrammeDefinition4 = new FestivalProgrammeDefinition {

    def getShowings: java.util.List[ShowingDefinition] = {
      val showings: java.util.LinkedList[ShowingDefinition] = new java.util.LinkedList[ShowingDefinition]()
      val movie: MovieMock = new MovieMock(10, Period.minutes(60))
      showings.add(new ShowingMock(15, 100, DateTime.parse("2012-06-01T10:30"), movie))
      showings.add(new ShowingMock(25, 101, DateTime.parse("2012-06-01T11:00"), movie))
      showings
    }
  }

  val festivalProgramme4 = new FestivalProgramme(festivalProgrammeDefinition4)

  val tiff2012ProgrammeDefinition = new FestivalProgrammeDefinition {

    val showings: util.List[ShowingDefinition] = new FestivalProgrammeXmlParser("/tiff_2012_programme.xml").showings

    def getShowings: util.List[ShowingDefinition] = {
      showings
    }
  }

  val tiff2012Programme = new FestivalProgramme(tiff2012ProgrammeDefinition)
}
