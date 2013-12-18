/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._
import scala.collection.immutable.Queue

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection*/
  case object GC

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply
  
  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}

class BinaryTreeSet extends Actor {
  import BinaryTreeSet._
  import BinaryTreeNode._

  var index = 0
  def createRoot: ActorRef = { index += 1; context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true), "root-"+index) }

  var root = createRoot

  // optional
  var pendingQueue = Queue.empty[Operation]

  // optional
  def receive = normal

  // optional
  /** Accepts `Operation` and `GC` messages. */
  def normal: Receive = {
    case operation: Operation => root ! operation
    case GC => {
      val newRoot = createRoot
      root ! CopyTo(newRoot)
      context.become(garbageCollecting(newRoot))
    }
  }

  // optional
  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = {
    case operation: Operation => pendingQueue = pendingQueue.enqueue(operation)
    case CopyFinished => {
      context.stop(root)
      root = newRoot
      pendingQueue foreach { root ! _ }
      pendingQueue = Queue.empty[Operation]
      context.become(normal)
    }
  }

}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode],  elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  // optional
  def receive = normal

  def isActive = !removed
  def isLeaf = subtrees.isEmpty

  // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = {
    case op @ Insert  (requester, id, _) => handleTreeOp(op, insertNode,   { removed = false; requester ! OperationFinished(id)})
    case op @ Contains(requester, id, _) => handleTreeOp(op, containsNode, {                  requester ! ContainsResult(id, !removed)})
    case op @ Remove  (requester, id, _) => handleTreeOp(op, removeNode,   { removed = true;  requester ! OperationFinished(id)})
    case CopyTo(treeNode) => {
      if (isActive) {
        treeNode ! Insert(self, elem, elem)
      }
      if (isActive || !isLeaf) {
        subtrees foreach { case (_, actorRef) => actorRef ! CopyTo(treeNode) }
        context.become(copying(subtrees.values.toSet, insertConfirmed = !isActive))
      } else {
        context.parent ! CopyFinished
      }
    }
  }

  // optional
  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = {
    case OperationFinished(id) => {
      require(id == elem)
      if (expected.isEmpty) {
        context.parent ! CopyFinished
      } else {
        context.become(copying(expected, insertConfirmed = true))
      }
    }
    case CopyFinished => {
      require(expected.contains(sender))
      context.stop(sender)
      if (expected.size == 1 && insertConfirmed) {
        context.parent ! CopyFinished
      } else {
        context.become(copying(expected - sender, insertConfirmed))
      }
    }
  }

  def handleTreeOp(op: Operation, onNotFound: (Operation, Position) => Unit, onFound: => Unit) = {
    if (op.elem < elem) {
      onNotFound(op, Left)
    } else if (op.elem > elem) {
      onNotFound(op, Right)
    } else {
      onFound
    }
  }

  def insertNode(operation: Operation, position: Position) = {
    subtrees.get(position) match {
      case Some(subtreeRoot) => subtreeRoot ! operation
      case None => {
        subtrees += position -> context.actorOf(props(operation.elem, initiallyRemoved = false), "node-"+operation.elem)
        operation.requester ! OperationFinished(operation.id)
      }
    }
  }

  def containsNode(operation: Operation, position: Position) = {
    subtrees.get(position) match {
      case Some(subtreeRoot) => subtreeRoot ! operation
      case None => {
        operation.requester ! ContainsResult(operation.id, result = false)
      }
    }
  }

  def removeNode(operation: Operation, position: Position) = {
    subtrees.get(position) match {
      case Some(subtreeRoot) => subtreeRoot ! operation
      case None => {
        operation.requester ! OperationFinished(operation.id)
      }
    }
  }

}
