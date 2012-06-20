package org.ffplanner.scripts

import java.io.{ByteArrayOutputStream, OutputStream, FileOutputStream}
import java.net.{URLEncoder, URL}
import java.nio.file.{Paths, Path}
import org.xml.sax.InputSource
import xml.Node
import org.ffplanner.entity.Movie
import util.parsing.json.JSON

/**
 *
 * @author Bogdan Dumitriu
 */
object ScalaUtils {

  def getMovieInfoFromImdb(movie: Movie): Map[String, Any] = {
    val imdbPageAsJson = downloadPage(getImdbApiLinkFor(movie))
    val movieData: Option[Any] = JSON.parseFull(imdbPageAsJson)
    movieData.get.asInstanceOf[Map[String, Any]]
  }

  private def getImdbApiLinkFor(movie: Movie): String = {
    val addressBuilder = new StringBuilder("http://www.imdbapi.com/?")
    addressBuilder.append("t=")
    addressBuilder.append(URLEncoder.encode(movie.getOriginalTitle, "utf-8"))
    val movieYear: String = movie.getYear
    if (movieYear != null && !movieYear.isEmpty) {
      addressBuilder.append("&y=")
      addressBuilder.append(URLEncoder.encode(movie.getYear, "utf-8"))
    }
    addressBuilder.toString()
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

  def downloadPage(address: String): String = {
    val outputStream: ByteArrayOutputStream = new ByteArrayOutputStream()
    try {
      downloadPage(address, outputStream)
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
}
