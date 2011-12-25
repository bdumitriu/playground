package org.ffplanner.scripts

import org.htmlcleaner.HtmlCleaner
import org.htmlcleaner.SimpleXmlSerializer
import static java.nio.charset.StandardCharsets.UTF_8

/**
 *
 *
 * @author Bogdan Dumitriu
 */
class Utils {

    static def download(String address, String fileName) {
        def file = new File(fileName)
        def fileOutputStream = new FileOutputStream(file)
        def out = new BufferedOutputStream(fileOutputStream)
        out << new URL(address).openStream()
        out.close()

        def cleaner = new HtmlCleaner()
        def node = cleaner.clean(file, UTF_8.name())

        def props = cleaner.getProperties()
        def serializer = new SimpleXmlSerializer(props)
        serializer.writeToFile(node, fileName, UTF_8.name())

        return file
    }
}
