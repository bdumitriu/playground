import io.{Codec, Source}
import java.io._
import java.nio.charset.{StandardCharsets, Charset}

/**
 *
 * @author Bogdan Dumitriu
 */

object ChangeMkf extends App {

  val lines = Source.fromFile("d:\\work\\iga\\test-project\\mkf\\276SE35J.mkf", Codec.ISO8859.displayName()).getLines().toList
  val writer = new BufferedWriter(new OutputStreamWriter(
    new FileOutputStream("d:\\work\\iga\\test-project\\mkf\\276SE35J_alt.mkf"), StandardCharsets.ISO_8859_1))

  try {
    for (line <- lines) {
      line match {
        case l if (l.startsWith("M") || l.startsWith("16") || l.startsWith("600")) => writer.write(line)
        case _ => writer.write(getModifiedValues(line))
      }
      writer.write("\r\n")
    }
  } finally {
    writer.close()
  }

  def getModifiedValues(line: String): String = {
    val numbers = line.split('\t');
    val modifiedNumbers = numbers.map(n => (n.toDouble + 1.0).formatted("%.2f"))
    modifiedNumbers.mkString("\t")
  }
}
