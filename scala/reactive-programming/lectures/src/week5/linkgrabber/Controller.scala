package week5.linkgrabber

import akka.actor._
import scala.concurrent.duration._

/**
 *
 * @author Bogdan Dumitriu
 */
object Controller {
  case class Check(url: String, maxDepth: Int)
  case class Result(links: Set[String])
  case object Failed
//  private case object Timeout
}

class Controller extends Actor with ActorLogging {

  import Controller._

  var cache: Set[String] = Set.empty[String]

  var getters: Set[ActorRef] = Set.empty[ActorRef]
  // or rely on context.children

  context.setReceiveTimeout(10.seconds)

  // alternative
  //context.system.scheduler.scheduleOnce(10.seconds, self, Timeout)

  override val supervisorStrategy = OneForOneStrategy(maxNrOfRetries = 5) {
    case _: Exception => SupervisorStrategy.Restart
  }

  def receive: Receive = {
    case Check(url, depth) => {
      log.debug("{} checking {}", depth, url)
      if (!cache.contains(url) && depth > 0) {
        getters += context.actorOf(Props[Getter](new Getter(url, depth - 1)))
        // or...
        // context.watch(context.actorOf(Props[Getter](new Getter(url, depth - 1))))
      }
      cache += url
    }

    // or, using listeners...
    // case Terminated(_) => {
    //   if (context.children.isEmpty) {
    //     context.parent ! Result(cache)
    //   }
    // }

    case outcome @ (Getter.Done | Getter.Failed) => {
      getters -= sender
      if (getters.isEmpty) {
        context.parent ! (if (outcome == Getter.Failed && theOriginalUrlFailed) Failed else Result(cache))
      }
    }
    case ReceiveTimeout => {
      getters foreach (_ ! Getter.Abort)
      // or...
      // context.children foreach context.stop
    }

    // for the alternative timeout
    // case Timeout => {
    //   getters foreach (_ ! Getter.Abort)
    // }
  }

  private def theOriginalUrlFailed: Boolean = {
    cache.size == 1
  }
}
