package week5

import akka.actor._
import akka.testkit._
import scala.concurrent.duration._

class Toggle extends Actor {

  def happy: Receive = {
    case "How are you?" =>
      sender ! "happy"
      context become sad
  }

  def sad: Receive = {
    case "How are you?" =>
      sender ! "sad"
      context become happy
  }

  def receive = happy

}

object SimpleActorTest extends App {

  // running a TestProbe from the outside
  implicit val system = ActorSystem("TestSys")
  val toggle = system.actorOf(Props[Toggle])
  val p = TestProbe()
  p.send(toggle, "How are you?")
  p.expectMsg("happy")
  p.send(toggle, "How are you?")
  p.expectMsg("sad")
  p.send(toggle, "unknown")
  p.expectNoMsg(1.second)
  system.shutdown()

  // running inside a TestKit
  new TestKit(ActorSystem("TestSys")) with ImplicitSender {
    val toggle = system.actorOf(Props[Toggle])
    toggle ! "How are you?"
    expectMsg("happy")
    toggle ! "How are you?"
    expectMsg("sad")
    toggle ! "unknown"
    expectNoMsg(1.second)
    system.shutdown()
  }

  println("done")

}
