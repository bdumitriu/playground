package week5.linkgrabber

import akka.actor.{ActorRef, Props, Actor}

/**
 *
 * @author Bogdan Dumitriu
 */
object Receptionist {
  case class Get(url: String)
  case class Result(url: String, links: Set[String])
  case class Failed(url: String)
}

class Receptionist extends Actor {

  import Receptionist._

  var runNo = 0

  def controllerProps: Props = Props[Controller]

  def receive: Receive = waiting

  def waiting: Receive = {
    case Get(url) => {
      context.become(runNext(Vector(Job(sender, url))))
    }
  }

  def running(queue: Vector[Job]): Receive = {
    case Get(url) => {
      context.become(running(queue :+ Job(sender, url)))
    }
    case Controller.Result(links) => {
      val job = queue.head
      job.client ! Result(job.url, links)
      context.stop(sender)
      context.become(runNext(queue.tail))
    }
    case Controller.Failed => {
      val job = queue.head
      job.client ! Failed(job.url)
      context.stop(sender)
      context.become(runNext(queue.tail))
    }
  }

  def runNext(queue: Vector[Job]): Receive = {
    if (queue.isEmpty) {
      waiting
    } else {
      runNo += 1
      val controller = context.actorOf(controllerProps, s"controller-$runNo")
      controller ! Controller.Check(queue.head.url, 2)
      running(queue)
    }
  }

  case class Job(client: ActorRef, url: String)
/*
  object Job {
    def apply(client: ActorRef, url: String) = {
      new Job(client, url)
    }
  }*/
}
