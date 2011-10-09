package org.ffplanner

/**
 * @author Bogdan Dumitriu
 */
class TiffProcessor {

	private def also_download = false;

	private static movieCounter = 0;

	private static sectionCounter = 0;

	final def movieMap = [:]

	final def sectionMap = [:]

	def getSectionId(section) {
		def sectionId = sectionMap[section]
		if (sectionId == null) {
			sectionId = sectionCounter++
			sectionMap[section] = sectionId
		}
		return sectionId
	}

	def getMovieId(movieLink) {
		def movieId = movieMap[movieLink]
		if (movieId == null) {
			processMovie(movieLink)
			movieId = movieCounter++;
			movieMap[movieLink] = movieId
		}
		return movieId
	}

	def processMovie(movieLink) {
		def movieFile = downloadMovieFile("http://www.tiff.ro" + movieLink)
		def movieNode = new XmlParser(false, false).parse(movieFile)
		def movieEnglishTitle = movieNode.depthFirst().find {it?.@class == "h2_title"}.text()
		def movieDetailsNode = movieNode.depthFirst().find {it?.@class == "movie_list"}
		processMovieDetails(movieDetailsNode)
	}

	def processMovieDetails(movieDetailsNode) {
		def movieOriginalTitle = movieDetailsNode.ul.li[0].text()
		println movieOriginalTitle
		def movieCountriesAndDate = movieDetailsNode.ul.li[1].text().split(", ")
		def movieCountries = movieCountriesAndDate[0..-2]
		def movieDate = movieCountriesAndDate[-1][1..-2]
		def movieDirectors = movieDetailsNode.ul.li[2].text().split(", ")
		def movieCast = movieDetailsNode.ul.li[3].text().split(", ")
		def movieLength = movieDetailsNode.ul.li[4].text().replaceAll("\\s+", "")
		def movieDescription = movieDetailsNode.ul.li[5].p[0]
		print movieDescription
		println "\n"
	}

	def downloadMovieFile(address) {
		def addressTokens = address.tokenize("/");
		def fileName = addressTokens[-2] + "_" + addressTokens[-1] + ".html"
		if (also_download) {
			return Utils.download(address, fileName)
		} else {
			return new File(fileName)
		}
	}
}
