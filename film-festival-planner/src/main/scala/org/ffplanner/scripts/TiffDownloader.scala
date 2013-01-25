package org.ffplanner.scripts

import org.ffplanner.bean.{MovieBundleBean, MovieBean, ShowingBean}
import org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
import java.nio.file._
import java.text.SimpleDateFormat
import xml.{Node, XML}
import java.util
import java.nio.charset.StandardCharsets


/**
 *
 * @author Bogdan Dumitriu
 */
class TiffDownloader(showingBean: ShowingBean) {

  private val alsoDownload = true

  private var tiffMovies: TiffMovies = _

  def fillDatabaseFromSite(movieBean: MovieBean, movieBundleBean: MovieBundleBean) {
    tiffMovies = new TiffMovies(alsoDownload, movieBean, movieBundleBean)

    val path = Paths.get("tiff.html")
    if (alsoDownload && !Files.isRegularFile(path)) {
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
    val cells = showingNode.\("td")
    if (cells(2).text.trim == "Movie") {
      val showingHour = cells(0).text.trim
      val section = cells(3).text.trim
      val movieBundle = tiffMovies.getMovieBundle(getMovieLink(cells(1)), section)
      if (movieBundle.isDefined) {
        showingBean.createWith(movieBundle.get, day, showingHour, venue)
      }
    }
  }

  def getMovieLink(movieCell: Node) = {
    "http://www.tiff.ro" + (movieCell \ "a" \ "@href").text
  }
}
