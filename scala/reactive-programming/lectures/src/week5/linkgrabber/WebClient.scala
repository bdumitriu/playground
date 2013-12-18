package week5.linkgrabber

import java.util.concurrent.Executor
import scala.concurrent._
import com.ning.http.client.AsyncHttpClient

/**
 *
 * @author Bogdan Dumitriu
 */
trait WebClient {
  def get(url: String)(implicit exec: Executor): Future[WebClient.Result]
}

object WebClient {
  case class Result(originalOrRedirectedUrl: String, body: String)
}

case class BadStatus(status: Int) extends RuntimeException

object AsyncWebClient extends WebClient {
  import WebClient._

  private val client = new AsyncHttpClient()

  def get(url: String)(implicit exec: Executor): Future[Result] = {
    val future = client.prepareGet(url).execute()
    val promise = Promise[Result]()

    future.addListener(new Runnable {
      def run() = {
        val response = future.get
        if (response.getStatusCode == 302) {
          val links: Iterator[String] = LinkProcessor.findLinks(response.getResponseBodyExcerpt(131072))
          if (links.hasNext) {
            promise.completeWith(get(links.next()))
          } else {
            promise.success(Result(url, response.getResponseBodyExcerpt(1131072)))
          }
        } else if (response.getStatusCode < 400) {
          promise.success(Result(url, response.getResponseBodyExcerpt(1131072)))
        } else {
          throw new BadStatus(response.getStatusCode)
        }
      }
    }, exec)

    promise.future
  }

  def shutdown(): Unit = client.close()
}

object WebClientTest extends App {
  import scala.concurrent.ExecutionContext.Implicits.global
  AsyncWebClient get "http://www.google.com/" map println foreach (_ => AsyncWebClient.shutdown())
}
