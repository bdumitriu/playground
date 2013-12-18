package week5.linkgrabber

/**
 *
 * @author Bogdan Dumitriu
 */
object LinkProcessor {

  private val A_TAG = "(?i)<a([^>]+)>(.+?)</a>".r

  private val HREF_ATTR = """\s*(?i)href\s*=\s*(?:"([^"]*)"|'([^']*)'|([^'">\s]+))""".r

  def findLinks(body: String): Iterator[String] = {
    for {
      anchor <- A_TAG.findAllMatchIn(body)
      HREF_ATTR(dquot, quot, bare) <- anchor.subgroups
    } yield {
      if (dquot != null)
        dquot
      else if (quot != null)
        quot
      else
        bare
    }
  }
}
