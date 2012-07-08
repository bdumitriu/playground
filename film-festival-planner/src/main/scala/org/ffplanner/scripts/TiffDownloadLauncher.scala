package org.ffplanner.scripts

/**
 *
 * @author Bogdan Dumitriu
 */
object TiffDownloadLauncher extends App {

  new TiffDownloader(null).fillDatabaseFromSite(null, null)
}
