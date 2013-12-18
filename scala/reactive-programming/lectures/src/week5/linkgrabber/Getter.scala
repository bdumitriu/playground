package week5.linkgrabber

import akka.actor.{Status, Actor}
import akka.pattern.pipe
import java.util.concurrent.Executor
import scala.concurrent._

/**
 *
 * @author Bogdan Dumitriu
 */
object Getter {
  case class Failed(url: String)
  case object Abort
  case object Done
}

class Getter(url: String, depth: Int) extends Actor {

  import Getter._

  // the cast will not be necessary in future Akka releases
  implicit val exec = context.dispatcher.asInstanceOf[Executor with ExecutionContext]

  def webClient: WebClient = AsyncWebClient

  val future = webClient.get(url)
  /*future.onComplete {
    case Success(body) => self ! body
    case Failure(err) => self ! Status.Failure(err)
  }*/
  // the previous (commented) is equivalent to the following
  future.pipeTo(self)

  def receive: Receive = {
    case result: WebClient.Result => {
      for (link <- LinkProcessor.findLinks(result.body)) {
        context.parent ! Controller.Check(makeAbsolute(result.originalOrRedirectedUrl, link), depth)
      }
      stop()
    }
    case _: Status.Failure => {
      context.parent ! Failed(url)
      context.stop(self)
    }
    case Abort => {
      stop()
    }
  }

  def stop(): Unit = {
    context.parent ! Done
    context.stop(self)
  }

  private def makeAbsolute(url: String, link: String) = {
    if (link.startsWith("http://")) {
      link
    } else {
      val relativeLink = link.dropWhile(_ == '/')
      val absoluteUrl = if (url.endsWith("/")) url else url + '/'
      absoluteUrl + relativeLink
    }
  }
}
