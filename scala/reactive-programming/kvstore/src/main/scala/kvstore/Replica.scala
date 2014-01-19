package kvstore

import akka.actor.{ OneForOneStrategy, Props, ActorRef, Actor }
import kvstore.Arbiter._
import scala.collection.immutable.{SortedMap, Queue}
import akka.actor.SupervisorStrategy.Restart
import scala.annotation.tailrec
import akka.pattern.{ ask, pipe }
import akka.actor.Terminated
import scala.concurrent.duration._
import akka.actor.PoisonPill
import akka.actor.OneForOneStrategy
import akka.actor.SupervisorStrategy
import akka.util.Timeout
import akka.actor.FSM.->

object Replica {
  sealed trait Operation {
    def key: String
    def id: Long
  }
  case class Insert(key: String, value: String, id: Long) extends Operation
  case class Remove(key: String, id: Long) extends Operation
  case class Get(key: String, id: Long) extends Operation

  sealed trait OperationReply
  case class OperationAck(id: Long) extends OperationReply
  case class OperationFailed(id: Long) extends OperationReply
  case class GetResult(key: String, valueOption: Option[String], id: Long) extends OperationReply

  case class OperationTimeout(id: Long)

  def props(arbiter: ActorRef, persistenceProps: Props): Props = Props[Replica](new Replica(arbiter, persistenceProps))
}

class Replica(val arbiter: ActorRef, persistenceProps: Props) extends Actor {
  import Replica._
  import Replicator._
  import Persistence._
  import context.dispatcher

  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */

  var kv = Map.empty[String, String]
  // a map from secondary replicas to replicators
  var secondaries = Map.empty[ActorRef, ActorRef]
  // the current set of replicators
  var replicators = Set.empty[ActorRef]
  // a map from operation id to the client awaiting acknowledgement and the replicator actors that have to acknowledge beforehand
  var pending = Map.empty[Long, (ActorRef, Set[ActorRef])]
  // the next expected seq number
  var nextExpectedSeq = 0L
  // the persistence handler
  var persistence = context.actorOf(persistenceProps)
  // map from id to pair of sender and request
  var acks = Map.empty[Long, (ActorRef, Persist)]

  arbiter ! Join
  context.system.scheduler.schedule(100.millis, 100.millis, self, Timeout)

  override val supervisorStrategy = OneForOneStrategy() {
    case _: PersistenceException => Restart
  }

  def receive = {
    case JoinedPrimary   => context.become(leader)
    case JoinedSecondary => context.become(replica)
  }

  /* TODO Behavior for  the leader role. */
  val leader: Receive = {
    case Insert(key, value, id) => {
      kv += key -> value
      handleInsertRemove(key, Some(value), id)
    }
    case Remove(key, id) => {
      kv -= key
      handleInsertRemove(key, None, id)
    }
    case Get(key, id) => {
      sender ! GetResult(key, kv.get(key), id)
    }

    case Persisted(key, id) => {
      handlePersisted(key, id) map { case sender =>
        pending.get(id) map { case (client, reps) =>
          assert(client == sender)
          if (reps.isEmpty) {
            client ! OperationAck(id)
            pending -= id
          }
        }
      }
    }
    case Timeout => {
      acks foreach { case (_, (_, persistMessage)) => persistence ! persistMessage }
    }
    case OperationTimeout(id) => {
      pending.get(id) map { case (client, _) =>
        pending -= id
        acks -= id
        client ! OperationFailed(id)
      }
    }

    case Replicas(newReplicasWithSelf) => {
      val newReplicas = newReplicasWithSelf - self
      val oldReplicas: Set[ActorRef] = secondaries.keySet
      val incomingReplicas = newReplicas -- oldReplicas
      val outgoingReplicas = oldReplicas -- newReplicas
      val outgoingReplicators = (secondaries -- newReplicas).values.toSet
      incomingReplicas foreach { replica =>
        val replicator = context.actorOf(Replicator.props(replica))
        secondaries += replica -> replicator
        replicators += replicator
        sendKvsTo(replicator)
      }
      pending foreach { case (id, (client, reps)) => handleReplicated(id, client, reps, outgoingReplicators) }
      outgoingReplicators foreach { _ ! PoisonPill }
      secondaries --= outgoingReplicas
      replicators --= outgoingReplicators
    }

    case Replicated(key, id) => {
      pending.get(id) match {
        case None => //assert(assertion = false, "Unexpected ack!")
        case Some((client, reps)) => handleReplicated(id, client, reps, Set(sender))
      }
    }
  }

  /* TODO Behavior for the replica role. */
  val replica: Receive = {
    case Get(key, id) => {
      sender ! GetResult(key, kv.get(key), id)
    }

    case Snapshot(key, valueOption, seq) => {
      if (seq < nextExpectedSeq) {
        sender ! SnapshotAck(key, seq)
      } else if (seq == nextExpectedSeq) {
        triggerPersist(key, valueOption, seq)
        valueOption match {
          case Some(value) => kv += key -> value
          case None        => kv -= key
        }
        nextExpectedSeq = seq + 1
      }
    }

    case Persisted(key, seq) => {
      handlePersisted(key, seq) map { case sender => sender ! SnapshotAck(key, seq) }
    }
    case Timeout => {
      acks foreach { case (_, (_, persistMessage)) => persistence ! persistMessage }
    }
  }

  def handleInsertRemove(key: String, valueOption: Option[String], id: Long) {
    triggerPersist(key, valueOption, id)
    replicators foreach { _ ! Replicate(key, valueOption, id) }
    pending += id -> (sender, replicators)
    context.system.scheduler.scheduleOnce(1.second, self, OperationTimeout(id))
  }

  def handleReplicated(
    id: Long, client: ActorRef, pendingReplicators: Set[ActorRef], acknowledgingReplicators: Set[ActorRef]) {

    val newReps = pendingReplicators -- acknowledgingReplicators
    if (newReps.isEmpty && acks.get(id).isEmpty) {
      client ! OperationAck(id)
      pending -= id
    } else {
      pending = pending.updated(id, (client, newReps))
    }
  }

  def triggerPersist(key: String, valueOption: Option[String], id: Long) {
    val persistMessage = Persist(key, valueOption, id)
    persistence ! persistMessage
    acks += id -> (sender, persistMessage)
  }

  def handlePersisted(key: String, id: Long): Option[ActorRef] = {
    acks.get(id) map { case (sender, Persist(k, valueOption, seq)) =>
      assert(key == k)
      acks -= id
      sender
    }
  }

  def sendKvsTo(replicator: ActorRef) {
    kv.zipWithIndex foreach { case ((key, value), index) =>
      replicator ! Replicate(key, Some(value), index)

    }
  }

}
