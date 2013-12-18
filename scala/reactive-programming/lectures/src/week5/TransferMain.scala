package week5

import akka.actor.{Props, Actor}
import akka.event.LoggingReceive

/**
 *
 * @author Bogdan Dumitriu
 */
class TransferMain extends Actor {

  val accountA = context.actorOf(Props[BankAccount], "accountA")
  val accountB = context.actorOf(Props[BankAccount], "accountB")

  accountA ! BankAccount.Deposit(100)

  def receive: Receive = LoggingReceive {
    case BankAccount.Done => transfer(150)
  }

  def transfer(amount: BigInt): Unit = {
    val transaction = context.actorOf(Props[WireTransfer], "transfer")
    transaction ! WireTransfer.Transfer(accountA, accountB, amount)
    context.become(LoggingReceive {
      case WireTransfer.Done => {
        println("success!")
        context.stop(self)
      }
      case WireTransfer.Failed => {
        println("failure!")
        context.stop(self)
      }
    })
  }
}
