package org.ffplanner.scripts.tiff2011

import org.ffplanner.bean.ShowingBean
import org.ffplanner.bean.MovieBean
import org.ffplanner.bean.MovieBundleBean
import static org.ffplanner.scripts.Utils.download

/**
 * @author Bogdan Dumitriu
 */
class TiffDownloader {

    private def also_download = true;

    private TiffMovies tiffShowings

    ShowingBean showingBean

    def workYourMagic(ShowingBean showingBean, MovieBean movieBean, MovieBundleBean movieBundleBean) {
        this.showingBean = showingBean
        tiffShowings = new TiffMovies(also_download, movieBean, movieBundleBean)

        def file
        if (also_download && !(new File("tiff.html").exists())) {
            file = download("http://www.tiff.ro/en/program", "tiff.html")
        } else {
            file = new File("tiff.html")
        }

        def tiffProgramNode = new XmlSlurper(false, false).parse(file)
        tiffProgramNode.depthFirst().findAll {it.@class == "tableview"}.each {processDayAndVenueTable(it)}
    }

    def processDayAndVenueTable(dayVenueNode) {
        def dayAndVenue = dayVenueNode.tbody.tr[0].th.h3.text().split("\\|")*.trim()
        def day = dayAndVenue[0][dayAndVenue[0].indexOf('-') + 2..-1]
        def venue = dayAndVenue[1]
        dayVenueNode.tbody.tr[2..-1].each {processShowing(day, venue, it)}
    }

    def processShowing(day, venue, showingNode) {
        def section = showingNode.td[3].text()
        def movieBundle = tiffShowings.getMovieBundle(getMovieLink(showingNode), section)
        def showingHour = showingNode.td[0].text()
        showingBean.addShowing(movieBundle, day, showingHour, venue)
    }

    def getMovieLink(showingNode) {
        return "http://www.tiff.ro" + showingNode.td[1].a.@href.text()
    }
}
