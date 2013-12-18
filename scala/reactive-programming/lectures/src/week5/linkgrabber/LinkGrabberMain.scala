package week5.linkgrabber

import akka.actor.{ReceiveTimeout, Props, Actor}
import scala.concurrent.duration._

/**
 *
 * @author Bogdan Dumitriu
 */
class LinkGrabberMain extends Actor {

  import Receptionist._

  val receptionist = context.actorOf(Props[Receptionist], "receptionist")

  receptionist ! Get("http://www.google.com/")
  receptionist ! Get("https://e-justice.europa.eu/home.do")

  context.setReceiveTimeout(10.seconds)

  def receive: Receive = {
    case Receptionist.Result(url, links) => {
      println(links.toVector.sorted.mkString(s"Results for '$url':\n", "\n", "\n"))
    }
    case Receptionist.Failed(url) => {
      println(s"Failed to fetch '$url'\n")
    }
    case ReceiveTimeout => {
      context.stop(self)
    }
  }

  override def postStop(): Unit = {
    AsyncWebClient.shutdown()
  }
}
