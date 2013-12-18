package week3

import scala.language.postfixOps
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
import scala.collection.immutable.Queue
import akka.serialization._
import akka.actor._
import scala.util.Failure

trait NetworkStack {

  class URL(val url: String)

  object mailServer {
    val europe = new URL("mail.server.eu")
    val usa = new URL("mail.server.us")
  }

  class EMailMessage(val from: String, val to: String) extends Serializable

  object EMailMessage {
    def apply(from: String, to: String) = new EMailMessage(from, to)
  }

  class Request(payload: Array[Byte])

  class Response(bytes: Array[Byte]) {
    def isOk: Boolean = true
    def toByteArray: Array[Byte] = bytes
  }

  object Http {
    def apply(url: URL, req: Request): Future[Response] = Future {
      new Response("foo bar baz".toCharArray.map(_.toByte))
    }
  }

  trait Socket {
    def readFromMemory(): Future[Array[Byte]]
    def sendToEurope(packet: Array[Byte]): Future[Array[Byte]]
  }

  object Socket extends Socket {

    val memory = Queue[EMailMessage](
      EMailMessage(from = "Erik", to = "Roland"),
      EMailMessage(from = "Martin", to = "Erik"),
      EMailMessage(from = "Roland", to = "Martin")
    )

    def readFromMemory(): Future[Array[Byte]] = Future {
      val email: EMailMessage = memory.dequeue._1
      /*val serialization = SerializationExtension(ActorSystem("futures"))
      val serializer = serialization.findSerializerFor(email)
      serializer.toBinary(email)*/
      s"From: ${email.from}, To: ${email.to}".toCharArray.map(_.toByte)
    }

    def sendToEurope(packet: Array[Byte]): Future[Array[Byte]] =
      Http(mailServer.europe, new Request(packet)).filter(_.isOk).map(_.toByteArray)

    def sendTo(url: URL, packet: Array[Byte]): Future[Array[Byte]] =
      Http(url, new Request(packet)).filter(_.isOk).map(_.toByteArray)

    def sendToAndBackup(packet: Array[Byte]): Future[(Array[Byte], Array[Byte])] = { // crap
      val europeConfirm = sendTo(mailServer.europe, packet)
      val usaConfirm = sendTo(mailServer.usa, packet)
      europeConfirm.zip(usaConfirm)
    }

    def sendToSafe(packet: Array[Byte]): Future[Array[Byte]] =
      sendTo(mailServer.europe, packet) recoverWith {
        case europeError => sendTo(mailServer.europe, packet) recover {
          case usaError => usaError.getMessage.toCharArray.map(_.toByte)
        }
      }

    def sendToSafeWithEuropeError(packet: Array[Byte]): Future[Array[Byte]] =
      sendTo(mailServer.europe, packet) fallbackTo {
        sendTo(mailServer.europe, packet)
      } recover {
        case europeError => europeError.getMessage.toCharArray.map(_.toByte)
      }

/*
    def retry[T](noTimes: Int)(block: => Future[T]): Future[T] = {
      val failed: Future[Nothing] = Future.failed(new Exception("sorry"))
      (1 to noTimes).iterator.map(_ => () => block).foldLeft(failed) { (c, block) =>
        c recoverWith { block() }
      }
    }
*/

    def apply() = this
  }

  def getResult = {
    val socket = Socket()

    val c = for {
      packet <- socket.readFromMemory()
      confirmation <- socket.sendToEurope(packet)
    } yield confirmation

    Await.result(c, 2 seconds)
  }
}
