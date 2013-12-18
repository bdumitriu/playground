package week5.linkgrabber

import java.util.concurrent.Executor
import scala.concurrent.Future
import week5.linkgrabber.WebClient.Result
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.{WordSpecLike, BeforeAndAfterAll}

/**
 *
 * @author Bogdan Dumitriu
 */
class StepParent(child: Props, probe: ActorRef) extends Actor {
  context.actorOf(child, "child")

  def receive: Receive = {
    case msg => probe.tell(msg, sender)
  }
}

object GetterSpec {

  val firstLink = "http://www.bdumitriu.ro/1"

  val bodies = Map(
    firstLink ->
      """<html>
        |  <head><title>Page 1</title></head>
        |  <body>
        |    <h1>A link</h1>
        |    <a href="http://www.bdumitriu.ro/2">click here</a>
        |  </body>
        |</html>""".stripMargin)

  val links = Map(
    firstLink -> Seq("http://www.bdumitriu.ro/2")
  )

  object FakeWebClient extends WebClient {
    def get(url: String)(implicit exec: Executor): Future[Result] = {
      bodies get url match {
        case None       => Future.failed(BadStatus(404))
        case Some(body) => Future.successful(Result(url, body))
      }
    }
  }

  def fakeGetter(url: String, depth: Int): Props = {
    Props[Getter](new Getter(url, depth) {
      override def webClient: WebClient = FakeWebClient
    })
  }
}

class GetterSpec extends TestKit(ActorSystem("GetterSpec"))
  with WordSpecLike with BeforeAndAfterAll with ImplicitSender {

  import GetterSpec._

  override protected def afterAll(): Unit = {
    system.shutdown()
  }

  "A Getter" must {
    "return the right body" in {
      val getter = system.actorOf(Props[StepParent](new StepParent(fakeGetter(firstLink, 2), testActor)), "rightBody")
      for (link <- links(firstLink))
        expectMsg(Controller.Check(link, 2))
      expectMsg(Getter.Done)
    }

    "properly finish in case of errors" in {
      val getter = system.actorOf(Props[StepParent](new StepParent(fakeGetter("unknown", 2), testActor)), "wrongLink")
      expectMsg(Getter.Failed("unknown"))
    }
  }
}
