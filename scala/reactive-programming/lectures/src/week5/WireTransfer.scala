package week5

import akka.actor.{Actor, ActorRef}
import akka.event.LoggingReceive

/**
 *
 * @author Bogdan Dumitriu
 */
object WireTransfer {
  case class Transfer(from: ActorRef, to: ActorRef, amount: BigInt)
  case object Done
  case object Failed
}

class WireTransfer extends Actor {

  import WireTransfer._

  def receive: Receive = LoggingReceive {
    case Transfer(from, to, amount) => {
      from ! BankAccount.Withdraw(amount)
      context.become(awaitWithdraw(to, amount, sender))
    }
  }

  def awaitWithdraw(to: ActorRef, amount: BigInt, client: ActorRef): Receive = LoggingReceive {
    case BankAccount.Done => {
      to ! BankAccount.Deposit(amount)
      context.become(awaitDeposit(client))
    }
    case BankAccount.Failed => {
      client ! Failed
      context.stop(self)
    }
  }

  def awaitDeposit(client: ActorRef): Receive = LoggingReceive {
    case BankAccount.Done => {
      client ! Done
      context.stop(self)
    }
    case BankAccount.Failed => {
      client ! Failed
      context.stop(self)
    }
  }
}
