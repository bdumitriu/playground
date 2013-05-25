package org.ffplanner.scripts

import java.io._
import java.net.{URLEncoder, URL}
import java.nio.file.{Files, Paths, Path}
import org.xml.sax.InputSource
import xml.Node
import org.ffplanner.entity.Movie
import util.parsing.json.JSON
import scala.Some
import org.apache.commons.lang3.StringUtils

/**
 *
 * @author Bogdan Dumitriu
 */
object ScalaUtils {

  val DownloadDirectory = Paths.get("D:/tiff")

  val TemporaryImdbFile = DownloadDirectory.resolve("imdb_temp.json").toAbsolutePath.toString

  val CurlExecutable = Paths.get("C:/Program Files/Tools/curl/curl.exe").toAbsolutePath.toString

  def getMovieInfoFromImdb(movie: Movie): Map[String, Any] = {
    val imdbLink = getImdbLinkFor(movie)
    val imdbPageAsJson = downloadPageWithCurl(imdbLink)
    val imdbId = getImdbId(jsonToMap(imdbPageAsJson), movie.getYear)
    val omdbApiLink = if (imdbId.isEmpty) { getOmdbApiTitleLinkFor(movie) } else { getOmdbApiIdLinkFor(imdbId.get) }
    val omdbPageAsJson = downloadPage(omdbApiLink)
    jsonToMap(omdbPageAsJson)
  }

  private def getImdbId(movieData: Map[String, Any], year: String): Option[String] = {
    getMovieFromImdbReply(movieData.get("title_exact"), year) match {
      case Some(movie) => movie.get("id").asInstanceOf[Option[String]]
      case None => getMovieFromImdbReply(movieData.get("title_approx"), year) match {
        case Some(movie) => movie.get("id").asInstanceOf[Option[String]]
        case None => None
      }
    }
  }

  private def getMovieFromImdbReply(movies: Option[Any], year: String): Option[Map[String, Any]] = movies match {
    case Some(list) => list.asInstanceOf[List[Map[String, Any]]].filter(_("title_description").asInstanceOf[String].startsWith(year)).headOption
    case None => None
  }

  private def getImdbLinkFor(movie: Movie): String = {
    val movieTitle = URLEncoder.encode(movie.getOriginalTitle, "utf-8")
    s"http://www.imdb.com/xml/find?json=1&nr=1&tt=on&q=$movieTitle"
  }

  private def getOmdbApiTitleLinkFor(movie: Movie): String = {
    val movieTitle = URLEncoder.encode(movie.getOriginalTitle, "utf-8")
    val movieYear: String = URLEncoder.encode(StringUtils.defaultString(movie.getYear), "utf-8")
    s"http://www.omdbapi.com/?t=${movieTitle}${if (StringUtils.isBlank(movieYear)) { "" } else { s"&y=$movieYear " } }"
  }

  private def getOmdbApiIdLinkFor(imdbId: String): String = s"http://www.omdbapi.com/?i=$imdbId"

  private def jsonToMap(json: String): Map[String, Any] = {
    JSON.parseFull(json).get.asInstanceOf[Map[String, Any]]
  }

  def hasAttributeEqualTo(node: Node, name: String, value: String): Boolean = {
    node.attribute(name).exists(_.text == value)
  }

  def hasAttributeContaining(node: Node, name: String, value: String): Boolean = {
    node.attribute(name).exists(_.text.contains(value))
  }

  def missesAttribute(node: Node, name: String): Boolean = {
    node.attribute(name).isEmpty
  }

  def downloadPage(address: String, path: Path) {
    val outputStream: FileOutputStream = new FileOutputStream(path.toFile)
    try {
      downloadPage(address, outputStream)
    } finally {
      outputStream.close()
    }
  }

  def downloadPage(address: String): String = downloadPageWith(address, downloadPage)

  def downloadPageWithCurl(address: String): String = downloadPageWith(address, downloadPageWithCurl)

  private def downloadPageWith(address: String, f: (String, OutputStream) => Unit): String = {
    val outputStream: ByteArrayOutputStream = new ByteArrayOutputStream()
    try {
      f(address, outputStream)
    } catch {
      case e: Exception => e.printStackTrace()
    } finally {
      outputStream.close()
    }
    outputStream.toString
  }

  private def downloadPage(address: String, outputStream: OutputStream) {
    val url = new URL(address)
    val connection = url.openConnection()
    val inputStream = new InputSource(connection.getInputStream).getByteStream
    try {
      val buffer = new Array[Byte](131072)
      Iterator.continually(inputStream.read(buffer)).takeWhile(_ != -1).foreach(outputStream.write(buffer, 0, _))
    } finally {
      inputStream.close()
    }
  }

  private def downloadPageWithCurl(address: String, outputStream: OutputStream) {
    val process: Process = new ProcessBuilder(CurlExecutable, address, "-o", TemporaryImdbFile).start()
    process.waitFor()
    val inputStream = new InputSource(new FileInputStream(TemporaryImdbFile)).getByteStream
    try {
      val buffer = new Array[Byte](131072)
      Iterator.continually(inputStream.read(buffer)).takeWhile(_ != -1).foreach(outputStream.write(buffer, 0, _))
    } finally {
      inputStream.close()
      Files.delete(Paths.get(TemporaryImdbFile))
    }
  }
}
