package kvstore

import akka.actor.Props
import akka.actor.Actor
import akka.actor.ActorRef
import scala.concurrent.duration._
import akka.actor.FSM.->
import akka.util.Timeout
import scala.collection.immutable.SortedMap

object Replicator {
  case class Replicate(key: String, valueOption: Option[String], id: Long)
  case class Replicated(key: String, id: Long)
  
  case class Snapshot(key: String, valueOption: Option[String], seq: Long)
  case class SnapshotAck(key: String, seq: Long)

  def props(replica: ActorRef): Props = Props[Replicator](new Replicator(replica))
}

class Replicator(val replica: ActorRef) extends Actor {
  import Replicator._
  import Replica._
  import context.dispatcher

  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */

  // map from sequence number to pair of sender and request
  var acks = SortedMap.empty[Long, (ActorRef, Replicate)]
  // a sequence of not-yet-sent snapshots (you can disregard this if not implementing batching)
  var pending = Vector.empty[Snapshot]

  context.system.scheduler.schedule(100.millis, 100.millis, self, Timeout)

  var _seqCounter = 0L
  def nextSeq = {
    val ret = _seqCounter
    _seqCounter += 1
    ret
  }
  
  /* TODO Behavior for the Replicator. */
  def receive: Receive = {
    case r @ Replicate(key, valueOption, id) => {
      val seq = nextSeq
      replica ! Snapshot(key, valueOption, seq)
      acks += seq -> (sender, r)
    }
    case SnapshotAck(key, seq) => {
      val ackOption = acks.get(seq)
      if (ackOption.isDefined) {
        val ack = ackOption.get
        ack._1 ! Replicated(key, ack._2.id)
        acks -= seq
      }
    }
    case Timeout => {
      acks foreach { case (seq, (_, Replicate(key, valueOption, _))) => replica ! Snapshot(key, valueOption, seq) }
    }
  }

}
