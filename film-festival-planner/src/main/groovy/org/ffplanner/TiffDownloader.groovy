package org.ffplanner

import groovy.util.slurpersupport.*

/**
 * @author Bogdan Dumitriu
 */
class TiffDownloader {

	private TiffProcessor tiffProcessor = new TiffProcessor()

	public static void main(String[] args) {
		new TiffDownloader().workYourMagic()
	}

	def workYourMagic() {
		def file = new File("tiff.html")
		//def file = download("http://www.tiff.ro/en/program", "tiff.html")

		def tiffProgramNode = new XmlSlurper(false, false).parse(file)
		tiffProgramNode.depthFirst().findAll {it.@class.text() == "tableview"}.each {processDayAndCinemaTable(it)}
	}

	def processDayAndCinemaTable(dayCinemaNode) {
		def dayAndCinema = dayCinemaNode.tbody.tr[0].th.h3.text().split("\\|")*.trim()
		def day = dayAndCinema[0][dayAndCinema[0].indexOf('-')+2..-1]
		def cinema = dayAndCinema[1]
		dayCinemaNode.tbody.tr[2..-1].each {processShowing(it)}
	}

	def processShowing(showingNode) {
		def movieId = tiffProcessor.getMovieId(getMovieLink(showingNode))
		def showingHour = showingNode.td[0].text()
		def sectionId = tiffProcessor.getSectionId(showingNode.td[3].text())
	}

	def getMovieLink(showingNode) {
		return showingNode.td[1].a.@href.text()
	}
}
