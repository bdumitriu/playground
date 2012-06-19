package org.ffplanner.scripts

import java.io.FileOutputStream
import java.net.URL
import java.nio.file.Path
import org.xml.sax.InputSource
import xml.Node

/**
 *
 * @author Bogdan Dumitriu
 */
object ScalaUtils {

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
    val url = new URL(address)
    val connection = url.openConnection()
    val inputStream = new InputSource(connection.getInputStream).getByteStream
    try {
      val outputStream = new FileOutputStream(path.toFile)
      try {
        val buffer = new Array[Byte](131072)
        Iterator.continually(inputStream.read(buffer)).takeWhile(_ != -1).foreach(outputStream.write(buffer, 0, _))
      } finally {
        outputStream.close()
      }
    } finally {
      inputStream.close()
    }
  }
}
