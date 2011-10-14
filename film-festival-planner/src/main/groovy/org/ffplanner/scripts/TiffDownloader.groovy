package org.ffplanner.scripts

import javax.ejb.embeddable.EJBContainer
import javax.naming.Context
import org.ffplanner.ShowingEJB
import static org.ffplanner.scripts.Utils.download

/**
 * @author Bogdan Dumitriu
 */
class TiffDownloader {

	private def also_download = false;

	private TiffMovies tiffShowings

	EJBContainer ejbContainer

	Context context

	ShowingEJB showingEJB

	public static void main(String[] args) {
		new TiffDownloader().workYourMagic()
	}

	def workYourMagic() {
		initEJBContainer()
		try {
			this.showingEJB = (ShowingEJB) context.lookup("java:global/classes/ShowingEJB");
			tiffShowings = new TiffMovies(also_download, context)

			def file
			if (also_download) {
				file = download("http://www.tiff.ro/en/program", "tiff.html")
			} else {
				file = new File("tiff.html")
			}

			def tiffProgramNode = new XmlSlurper(false, false).parse(file)
			tiffProgramNode.depthFirst().findAll {it.@class == "tableview"}.each {processDayAndVenueTable(it)}
		} finally {
			ejbContainer.close()
		}
	}

	def initEJBContainer() {
		final Map<String, Object> properties = new HashMap<String, Object>();
		properties.put(EJBContainer.MODULES, new File("target/classes"));
		properties.put("org.glassfish.ejb.embedded.glassfish.installation.root",
				"C:\\Users\\bdumitriu\\glassfish3\\glassfish");
		ejbContainer = EJBContainer.createEJBContainer(properties)
		context = ejbContainer.getContext()
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
		showingEJB.addShowing(movieBundle, day, showingHour, venue)
	}

	def getMovieLink(showingNode) {
		return "http://www.tiff.ro" + showingNode.td[1].a.@href.text()
	}
}
