package org.ffplanner.scripts

import org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
import java.nio.file._
import xml.{Node, XML}
import java.util
import java.nio.charset.StandardCharsets
import org.ffplanner.util.ConstantsToGetRidOf
import java.text.SimpleDateFormat

/**
  * @author Bogdan Dumitriu
  */
object Config {

  val FestivalEditionId = ConstantsToGetRidOf.DEFAULT_FESTIVAL_EDITION_ID

  val AlsoDownload = true

  val DryRun = false
}

class TiffDownloader(beanBundle: BeanBundle) {

  private val tiffMovies: TiffMovies = new TiffMovies(beanBundle)

  private var nrShowingToProcess = Int.MaxValue

  def fillDatabaseFromSite() {
    val path = ScalaUtils.DownloadDirectory.resolve("tiff.html")
    if (Config.AlsoDownload && !Files.isRegularFile(path)) {
      ScalaUtils.downloadPage("http://www.tiff.ro/en/program", path)
    }

    val parser = XML.withSAXParser(new SAXFactoryImpl().newSAXParser())
    val tiffProgramNode: Node = parser.load(Files.newBufferedReader(path, StandardCharsets.UTF_8))

    tiffProgramNode \\ "_" filter {_.\("@class").text == "view-grouping"} map {processDay(_)}
  }

  def processDay(node: Node) {
    val dayString = node.\("div")(0).\("div")(1).\("span").\("@content").text
    val day = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss").parse(dayString)
    val contentNode = node.\("div")(1)
    val venues = contentNode \ "div" filter {_.\("@class").text == "locatie"}
    val venueMovies = contentNode \ "table" filter {_.\("@class").isEmpty}
    venues zip venueMovies map {nodePair => processVenueTable(day, nodePair._1.text, nodePair._2)}
  }

  def processVenueTable(day: util.Date, venue: String, venueMovies: Node) {
    venueMovies \ "tbody" \ "tr" map {processShowing(day, venue, _)}
  }

  def processShowing(day: util.Date, venue: String, showingNode: Node) {
    if (nrShowingToProcess > 0) {
      val cells = showingNode.\("td")
      if (cells(2).text.trim == "Movie") {
        val showingHour = cells(0).text.trim
        val section = cells(3).text.trim
        if (!Config.DryRun){
          println(s"Processing ${getMovieLink(cells(1))}...")
        }
        val movieBundle = tiffMovies.getMovieBundle(getMovieLink(cells(1)), section)
        if (movieBundle.isDefined && !Config.DryRun) {
          try {
            beanBundle.showingBean.createWith(movieBundle.get, Config.FestivalEditionId, day, showingHour, venue)
          } catch {
            case e: Exception =>
              println("=== <ERROR-processShowing> ===")
              println(e.getMessage)
              println("=== </ERROR-processShowing> ===")
              throw e
          }
        }
      }
      nrShowingToProcess -= 1
    }
  }

  def getMovieLink(movieCell: Node) = {
    "http://www.tiff.ro" + (movieCell \ "a" \ "@href").text
  }
}
