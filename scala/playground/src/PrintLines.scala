import io.Source

/**
 *
 * @author Bogdan Dumitriu
 */

object PrintLines extends App {

  val lines = Source.fromFile("src/PrintLines.scala").getLines().toList
  val maxWidth = widthOfLength(lines.reduceLeft((a, b) => if (a.length < b.length) b else a))
  for (line <- lines) {
    val numSpaces = maxWidth - widthOfLength(line)
    val padding = " " * numSpaces
    println(padding + line.length + "| " + line)
  }

  def widthOfLength(line: String): Int = line.length.toString.length
}
