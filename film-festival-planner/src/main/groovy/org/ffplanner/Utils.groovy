package org.ffplanner

import org.htmlcleaner.*

/**
 *
 *
 * @author Bogdan Dumitriu
 */
class Utils {

	static def download(address, fileName) {
		def file = new File(fileName)
		def fileOutputStream = new FileOutputStream(file)
		def out = new BufferedOutputStream(fileOutputStream)
		out << new URL(address).openStream()
		out.close()

		def cleaner = new HtmlCleaner()
		def node = cleaner.clean(file)

		def props = cleaner.getProperties()
		def serializer = new SimpleXmlSerializer(props)
		def xml = serializer.getXmlAsString(node)

		fileOutputStream = new FileOutputStream(file)
		out = new BufferedOutputStream(fileOutputStream)
		out << xml
		out.close()

		return file
	}
}
