package org.ffplanner.scripts

import org.ffplanner.bean.{MovieBundleBean, MovieBundleInFestivalBean, MovieBean}
import java.nio.file.{Files, Path}
import org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
import xml.{XML, Node}
import collection.JavaConversions
import java.lang.Iterable
import org.ffplanner.entity.{Movie, MovieBundle}
import java.util
import java.nio.charset.StandardCharsets
import org.ffplanner.util.ConstantsToGetRidOf
import org.apache.commons.lang3.text.WordUtils

/**
 *
 * @author Bogdan Dumitriu
 */
class TiffMovies(beanBundle: BeanBundle) {

  private val TitleAndCountries = """(.*) \((.*)\)""".r

  private var movieBundleMap = Map[String, MovieBundle]()

  private var movieMap = Map[String, Movie]()

  def getMovieBundle(movieBundleLink: String, section: String): Option[MovieBundle] = {
    try {
      Some(movieBundleMap(movieBundleLink))
    } catch {
      case e: util.NoSuchElementException => {
        val movieBundle = processMovieOrMovieBundle(movieBundleLink, processMovieOrMovieBundleDetails(section))
        if (movieBundle.isDefined) {
          movieBundleMap += (movieBundleLink -> movieBundle.get)
        }
        movieBundle
      }
    }
  }

  def getMovie(movieLink: String): Option[Movie] = {
    try {
      Some(movieMap(movieLink))
    } catch {
      case e: util.NoSuchElementException => {
        val movie = processMovieOrMovieBundle(movieLink, processMovieDetails)
        if (movie.isDefined) {
          movieMap += (movieLink -> movie.get)
        }
        movie
      }
    }
  }

  def processMovieOrMovieBundle[A](link: String, processFunction: (String, Node) => A): Option[A] = {
    try {
      val path = downloadMovieFile(link)
      val parser = XML.withSAXParser(new SAXFactoryImpl().newSAXParser())
      val movieNode: Node = parser.load(Files.newBufferedReader(path, StandardCharsets.UTF_8))
      val movieBundleEnglishTitle = movieNode.\\("h1").text
      val singleNodeSeq = movieNode \\ "div" filter {
        _.\("@class").text.contains("node-film")
      }
      Some(processFunction(movieBundleEnglishTitle, singleNodeSeq.head))
    } catch {
      case e: Exception => None
    }
  }

  def processMovieOrMovieBundleDetails(section: String)(movieBundleEnglishTitle: String, movieBundleDetailsNode: Node):
    MovieBundle = {

    val TitleAndCountries(movieBundleOriginalTitle, _) = movieBundleDetailsNode.\("div").\("div")(0).text
    var movies = List[Movie]()
    val synopsisNode = movieBundleDetailsNode \\ "div" filter {
      _.\("@class").text.contains("synopsis")
    }
    val movieLinks = synopsisNode \\ "a" filter {
      _.\("@href").text.startsWith("http://tiff.ro/en/tiff/film")
    } map {
      _.\("@href").text
    }
    movieLinks.reverseMap({ link: String =>
      movies = getMovie(link).toList ::: movies
    })
    if (movieLinks.isEmpty) {
      movies = processMovieDetails(movieBundleEnglishTitle, movieBundleDetailsNode) :: movies
    }

    val movieBundle = new MovieBundle()
    if (!movieLinks.isEmpty) {
      movieBundle.setEnglishTitle(movieBundleEnglishTitle)
      movieBundle.setOriginalTitle(movieBundleOriginalTitle)
    }
    if (Config.DryRun) {
      println("adding movie bundle "+movieBundle.getEnglishTitle+" / "+movieBundle.getOriginalTitle)
      println()
    } else {
      try {
        beanBundle.movieBundleBean.createWith(movieBundle, JavaConversions.asJavaCollection(movies.map(_.getId)))
        beanBundle.movieBundleInFestivalBean.addMovieBundleInFestival(
          movieBundle.getId, Config.FestivalEditionId, section)
      } catch {
        case e: Exception =>
          println("=== <ERROR-processMovieOrMovieBundleDetails> ===")
          println(e.getMessage)
          println("=== </ERROR-processMovieOrMovieBundleDetails> ===")
          throw e
      }
    }
    movieBundle
  }

  def processMovieDetails(movieEnglishTitle: String, movieDetailsNode: Node): Movie = {
    val TitleAndCountries(movieOriginalTitle, movieCountries) = movieDetailsNode.\("div").\("div")(0).text
    val movieSpecsNode = movieDetailsNode.\("div").\("div")(1).\("div")(1)
    val movieYear = getAttributeIfAvailable(movieSpecsNode, "geb")
    val movieDirectors = getAttributeIfAvailable(movieSpecsNode, "regia")
    val movieCast = getAttributeIfAvailable(movieSpecsNode, "cast")
    val movieDescription = cleanSynopsis(getAttributeIfAvailable(movieSpecsNode, "synopsis"))

    val movie = new Movie()
    movie.setEnglishTitle(movieEnglishTitle)
    movie.setOriginalTitle(movieOriginalTitle)
    movie.setYear(movieYear)
    movie.setDescription(movieDescription)

    val movieInfo = ScalaUtils.getMovieInfoFromImdb(movie)
    try {
      val durationString: String = movieInfo("Runtime").asInstanceOf[String]
      if (durationString != "N/A") {
        movie.setDuration(durationString.replace("h", "h:").replace("min", "m").replaceAll("\\s+", ""))
      } else {
        movie.setDuration("00h:00m")
      }
    } catch {
      case e: util.NoSuchElementException => movie.setDuration("00h:00m")
    }
    try {
      movie.setImdbId(movieInfo("imdbID").asInstanceOf[String])
    } catch {
      case e: util.NoSuchElementException =>
    }

    if (Config.DryRun) {
      println("adding movie "+movie.getEnglishTitle+" / "+movie.getOriginalTitle+" / "+movie.getYear)
      println("  directors  : "+movieDirectors)
      println("  cast       : "+movieCast)
      println("  countries  : "+movieCountries)
      println("  duration   : "+movie.getDurationInMinutes)
      println("  imdbID     : "+movie.getImdbId)
      println("  description: ")
      println(WordUtils.wrap(movie.getDescription, 100))
      println()
    } else {
      try {
        beanBundle.movieBean.createWith(
          movie, toIterable(movieDirectors), toIterable(movieCast), toIterable(movieCountries))
      } catch {
        case e: Exception =>
          println("=== <ERROR-processMovieDetails> ===")
          println(e.getMessage)
          println("=== </ERROR-processMovieDetails> ===")
          throw e
      }
    }
    movie
  }

  def cleanSynopsis(synopsis: String): String = {
    val cutSynopsis = Array(
      "festivals:",
      "festivals\n",
      "*screening",
      "awards:",
      "awards\n",
      "you can purchase tickets here",
      "you can purchase tickets here\n",
      "here you can purchase your tickets",
      "here you can purchase your tickets\n"
    ).foldLeft(synopsis)({
      cutBefore(_, _)
    })
    cutSynopsis.replaceAll("\\s+", " ").trim
  }

  def cutBefore(string: String, searchTerm: String): String = {
    val idx = string.toLowerCase.indexOf(searchTerm)
    if (idx != -1) {
      string.substring(0, idx)
    } else {
      string
    }
  }

  def getAttributeIfAvailable(movieSpecsNode: Node, attribute: String): String = {
    val seqSingleContainerDiv = movieSpecsNode.\\("div").filter({
      _.\("@class").text.contains(attribute)
    })
    try {
      seqSingleContainerDiv.\("div")(1).text
    } catch {
      case e: IndexOutOfBoundsException => ""
    }
  }

  def toIterable(commaSplitString: String): Iterable[String] = {
    val tokenArray = commaSplitString.trim.split(", ")
    if (tokenArray.size == 1 && tokenArray(0) == "") {
      JavaConversions.asJavaIterable(new Array[String](0))
    } else {
      JavaConversions.asJavaIterable(tokenArray)
    }
  }

  def downloadMovieFile(address: String): Path = {
    val addressTokens = address.split("/")
    val fileName = addressTokens(addressTokens.length - 1) + ".html"
    val path = ScalaUtils.DownloadDirectory.resolve(fileName)
    if (Config.AlsoDownload && !Files.isRegularFile(path)) {
      ScalaUtils.downloadPage(address, path)
    }
    path
  }
}
