package org.ffplanner.scripts.tiff2011

import org.ffplanner.bean.MovieBundleBean
import org.ffplanner.bean.MovieBean
import org.ffplanner.entity.Movie
import org.ffplanner.entity.MovieBundle
import static org.ffplanner.scripts.Utils.download

/**
 * @author Bogdan Dumitriu
 */
class TiffMovies {

    private def also_download;

    final def movieBundleMap = [:]

    final def movieMap = [:]

    MovieBean movieBean

    MovieBundleBean movieBundleBean

    def TiffMovies(also_download, MovieBean movieBean, MovieBundleBean movieBundleBean) {
        this.also_download = also_download
        this.movieBean = movieBean
        this.movieBundleBean = movieBundleBean
    }

    def getMovieBundle(movieBundleLink, section) {
        def movieBundle = movieBundleMap[movieBundleLink]
        if (movieBundle == null) {
            movieBundle =
                processMovieOrMovieBundle(movieBundleLink, this.&processMovieOrMovieBundleDetails.curry(section))
            movieBundleMap[movieBundleLink] = movieBundle
        }
        return movieBundle
    }

    def getMovie(movieLink) {
        def movie = movieMap[movieLink]
        if (movie == null) {
            movie = processMovieOrMovieBundle(movieLink, this.&processMovieDetails)
            movieMap[movieLink] = movie
        }
        return movie
    }

    def processMovieOrMovieBundle(link, p) {
        def movieFile = downloadMovieFile(link)
        def movieNode = new XmlParser(false, false).parse(movieFile)
        def movieEnglishTitle = movieNode.depthFirst().find {it?.@class == "h2_title"}.text()
        def movieDetailsNode = movieNode.depthFirst().find {it?.@class == "movie_list"}
        return p(movieEnglishTitle, movieDetailsNode)
    }

    def processMovieOrMovieBundleDetails(section, movieBundleEnglishTitle, movieBundleDetailsNode) {
        def movieBundleOriginalTitle = movieBundleDetailsNode.ul.li[0].text()
        def movies = []
        def movieBundleDescriptionNode = movieBundleDetailsNode.ul.li[5]
        def movieReferences = movieBundleDescriptionNode.depthFirst().findAll {it.@href != null}
        if (movieReferences.size() > 0) {
            movieReferences.each {
                movies += getMovie(it.@href)
            }
        } else {
            movies += processMovieDetails(movieBundleEnglishTitle, movieBundleDetailsNode)
        }

        final MovieBundle movieBundle = new MovieBundle()
        if (movieReferences.size() > 1) {
            movieBundle.setEnglishTitle(movieBundleEnglishTitle)
            movieBundle.setOriginalTitle(movieBundleOriginalTitle)
        }
        movieBundle.addMovies(movies)
        movieBundleBean.addShowing(movieBundle, section)
        return movieBundle
    }

    def processMovieDetails(movieEnglishTitle, movieDetailsNode) {
        def movieOriginalTitle = movieDetailsNode.ul.li[0].text()
        def movieCountriesAndYear = movieDetailsNode.ul.li[1].text().split(", ")
        def movieCountries = movieCountriesAndYear[0..-2]
        def movieYear = movieCountriesAndYear[-1][1..-2]
        def movieDirectors = movieDetailsNode.ul.li[2].text().split(", ")
        def movieCast = movieDetailsNode.ul.li[3].text().split(", ")
        String movieDuration = movieDetailsNode.ul.li[4].text().replaceAll("\\s+", "")
        def movieDescription = movieDetailsNode.ul.li[5].p[0]?.text()

        final Movie movie = new Movie()
        movie.setEnglishTitle(movieEnglishTitle)
        movie.setOriginalTitle(movieOriginalTitle)
        movie.setYear(movieYear)
        movie.setDescription(movieDescription)
        movie.setDuration(movieDuration)
        movieBean.addMovie(movie, Arrays.asList(movieDirectors), Arrays.asList(movieCast), movieCountries)
        return movie
    }

    def downloadMovieFile(address) {
        def addressTokens = address.tokenize("/");
        def fileName = addressTokens[-2] + "_" + addressTokens[-1] + ".html"
        if (also_download && !(new File(fileName).exists())) {
            return download(address, fileName)
        } else {
            return new File(fileName)
        }
    }
}
