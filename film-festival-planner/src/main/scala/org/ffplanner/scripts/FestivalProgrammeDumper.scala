package org.ffplanner.scripts

import javax.persistence.{EntityManager, Persistence}
import org.ffplanner.entity._
import org.ffplanner.util.ConstantsToGetRidOf
import scala.collection.JavaConversions.collectionAsScalaIterable
import xml.{Elem, PrettyPrinter}
import org.ffplanner.bean.{MovieBean, FestivalEditionBean, ShowingBean}
import java.io.{Writer, FileOutputStream}
import java.nio.channels.Channels

/** Writes an XML file with the movies and the showings from the database associated with
 *  [[org.ffplanner.util.ConstantsToGetRidOf.DEFAULT_FESTIVAL_EDITION_ID DEFAULT_FESTIVAL_EDITION_ID]]. Use
 *  `bin/festival-programme-dumper.bat` to run the script with the CLASSPATH properly set up.
 *
 *  @author Bogdan Dumitriu
 */
object FestivalProgrammeDumper extends App {

  if (args.length != 1) {
    println("Usage: java FestivalProgrammeDumper output.xml")
    sys.exit(1)
  }

  val entityManager: EntityManager = Persistence.createEntityManagerFactory("ffp_jdbc").createEntityManager()

  val festivalEdition: FestivalEdition = getFestivalEdition(entityManager)

  val xmlOutput: Elem =
    getXmlOutput(getMovies(entityManager, festivalEdition), getShowings(entityManager, festivalEdition))

  printXmlTo(args(0), xmlOutput)

  entityManager.close()

  def getMovies(entityManager: EntityManager, festivalEdition: FestivalEdition): Iterable[MovieBundleInFestival] = {
    val movieBean: MovieBean = new MovieBean()
    movieBean.setEntityManager(entityManager)
    movieBean.findBy(festivalEdition)
  }

  def getShowings(entityManager: EntityManager, festivalEdition: FestivalEdition): Iterable[Showing] = {
    val showingBean: ShowingBean = new ShowingBean()
    showingBean.setEntityManager(entityManager)
    showingBean.findBy(festivalEdition)
  }

  def getFestivalEdition(entityManager: EntityManager): FestivalEdition = {
    val festivalEditionBean: FestivalEditionBean = new FestivalEditionBean()
    festivalEditionBean.setEntityManager(entityManager)
    festivalEditionBean.find(ConstantsToGetRidOf.DEFAULT_FESTIVAL_EDITION_ID)
  }

  def getXmlOutput(movies: Iterable[MovieBundleInFestival], showings: Iterable[Showing]) = {
    <festival-programme>
      <movies>
        {movies.map { movie =>
        <movie>
          <id>{movie.getId}</id>
          <duration>{movie.getDuration.toStandardMinutes.getMinutes}</duration>
          <title>{movie.getMovieBundle.getOriginalTitle}</title>
        </movie>
        }}
      </movies>
      <showings>
        {showings.map { showing =>
        <showing>
          <id>{showing.getId}</id>
          <venue-id>{showing.getVenueId}</venue-id>
          <datetime>{showing.getDateTime}</datetime>
          <movie-id>{showing.getMovieBundleInFestival.getId}</movie-id>
          <title>{showing.getMovieBundleInFestival.getMovieBundle.getOriginalTitle}</title>
        </showing>
        }}
      </showings>
    </festival-programme>
  }

  def printXmlTo(fileName: String, xmlOutput: Elem) {
    val prettyPrinter: PrettyPrinter = new PrettyPrinter(120, 2)
    val fileOutputStream: FileOutputStream = new FileOutputStream(fileName)
    val writer: Writer = Channels.newWriter(fileOutputStream.getChannel, "UTF-8")
    try {
      writer.write("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
      writer.write(prettyPrinter.format(xmlOutput))
      writer.write("\n")
    } finally {
      writer.close()
    }
  }
}
