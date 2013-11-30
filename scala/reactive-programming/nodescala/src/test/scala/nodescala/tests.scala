package nodescala

import scala.language.postfixOps
import scala.util._
import scala.collection._
import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global
import scala.async.Async.async
import org.scalatest._
import NodeScala._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NodeScalaSuite extends FunSuite {

  test("A Future should always be created") {
    val always = Future.always(517)
    assert(Await.result(always, 1 second) == 517)
  }

  test("A Future should never be created") {
    val never = Future.never[Int]

    try {
      Await.result(never, 1 second)
      assert(false)
    } catch {
      case t: TimeoutException => // ok!
    }
  }

  test("List of no futures") {
    val futures = Future.all(Nil)
    assert(Await.result(futures, 1 second) == Nil)
  }

  test("List of one happy future") {
    val future = Future.always(10)
    val futures = Future.all(List(future))
    assert(Await.result(futures, 1 second) == List(10))
  }

  test("List of many happy futures") {
    val future1 = Future.always(10)
    val future2 = Future.always(100)
    val future3 = Future.always(1000)
    val futures = Future.all(List(future1, future2, future3))
    assert(Await.result(futures, 1 second) == List(10, 100, 1000))
  }

  test("List of one dark future") {
    val future = Future.failed(new Exception("game over"))
    val futures = Future.all(List(future))
    try {
      Await.result(futures, Duration.Inf)
      fail("Unexpected success!")
    } catch {
      case exception: Exception => assert("game over" == exception.getMessage)
    }
  }

  test("List of one dark future among many") {
    val future1 = Future.always(10)
    val future2 = Future.failed(new Exception("game over"))
    val future3 = Future.always(1000)
    val futures = Future.all(List(future1, future2, future3))
    try {
      Await.result(futures, Duration.Inf)
      fail("Unexpected success!")
    } catch {
      case exception: Exception => assert("game over" == exception.getMessage)
    }
  }

  test("List of any dark future") {
    val outcome = Array(0, 0, 0)
    for (i <- 1 to 100) {
      val randomFuture = Future.any(List(
        Future { throw new Exception("game over 1") },
        Future { throw new Exception("game over 2") },
        Future { throw new Exception("game over 3") }
      ))
      try {
        Await.result(randomFuture, Duration.Inf)
        fail("Unexpected success!")
      } catch {
        case exception: Exception =>
          if ("game over 1" == exception.getMessage) {
            outcome(0) += 1
          } else if ("game over 2" == exception.getMessage) {
            outcome(1) += 1
          } else if ("game over 3" == exception.getMessage) {
            outcome(2) += 1
          } else {
            fail("Unexpected case")
          }
      }
    }
//    outcome foreach { o => assert(o != 0) }
  }

  test("List of any happy future") {
    val outcome = Array(0, 0, 0)
    for (i <- 1 to 100) {
      val randomFuture = Future.any(List(Future { 1 }, Future { 2 }, Future { 3 }))
      try {
        val n = Await.result(randomFuture, Duration.Inf)
        if (n == 1) {
          outcome(0) += 1
        } else if (n == 2) {
          outcome(1) += 1
        } else if (n == 3) {
          outcome(2) += 1
        } else {
          fail("Success came in an unexpected way!")
        }
      } catch {
        case exception: Throwable => fail("Unexpected failure!")
      }
    }
//    outcome foreach { o => assert(o != 0) }
  }

  test("The future is properly delayed ") {
    val future = Future.delay(1 second)
    try {
      Await.ready(future, 600 millis)
    } catch {
      case t: TimeoutException => // ok!
    }
    try {
      Await.ready(future, 600 millis)
    } catch {
      case t: TimeoutException => fail("Future should have completed")
    }
  }

  test("The happy future is already known") {
    val future = Future { 1 }
    Await.ready(future, Duration.Inf)
    assert(1 == future.now)
  }

  test("The dark future is already known") {
    val future = Future { throw new Exception("game over") }
    try {
      Await.ready(future, Duration.Inf)
    } catch {
      case _: NoSuchElementException => // ok
    }
    try {
      future.now
    } catch {
      case exception: Exception => assert("game over" == exception.getMessage)
    }
  }

  test("The future is still unknown") {
    val future = Future.delay(1 minute)
    try {
      future.now
    } catch {
      case exception: NoSuchElementException => assert(true)
    }
  }

  test("A future checked for success") {
    val future = Future { 1 }
    val futureS = future continueWith { f => f.value }
    Await.ready(futureS, Duration.Inf)
    futureS.now.getOrElse {
      fail("Completed future was expected!")
    } match {
      case Success(n) => assert(1 == n)
      case Failure(_) => fail("Successful future was expected!")
    }
  }

  test("A future checked for failure") {
    val future = Future { throw new Exception("game over") }
    val futureS = future continueWith { f => f.value }
    Await.ready(futureS, Duration.Inf)
    futureS.now.getOrElse {
      fail("Completed future was expected!")
    } match {
      case Success(_) => fail("Failed future was expected!")
      case Failure(exception) => assert("game over" == exception.getMessage)
    }
  }

  test("A future checked for exception") {
    val future = Future { 1 }
    val futureS = future continueWith { f => throw new Exception("game over") }
    Await.ready(futureS, Duration.Inf)
    futureS.value.getOrElse {
      fail("Completed future was expected!")
    } match {
      case Success(_) => fail("Failed future was expected!")
      case Failure(exception) => assert("game over" == exception.getMessage)
    }
  }

  test("CancellationTokenSource should allow stopping the computation") {
    val cts = CancellationTokenSource()
    val ct = cts.cancellationToken
    val p = Promise[String]()

    async {
      while (ct.nonCancelled) {
        // do work
      }

      p.success("done")
    }

    cts.unsubscribe()
    assert(Await.result(p.future, 1 second) == "done")
  }

  test("Canceling a future") {
    val p = Promise[Unit]()
    val working = Future.run() { ct =>
      Future {
        while (ct.nonCancelled) {
        }
        p.success(())
      }
    }
    working.unsubscribe()

    try {
      Await.ready(p.future, 1 second)
      assert(true)
    } catch {
      case _: Throwable => fail("Future should have completed!")
    }
  }

  class DummyExchange(val request: Request) extends Exchange {
    @volatile var response = ""
    val loaded = Promise[String]()
    def write(s: String) {
      response += s
    }
    def close() {
      loaded.success(response)
    }
  }

  class DummyListener(val port: Int, val relativePath: String) extends NodeScala.Listener {
    self =>

    @volatile private var started = false
    var handler: Exchange => Unit = null

    def createContext(h: Exchange => Unit) = this.synchronized {
      assert(started, "is server started?")
      handler = h
    }

    def removeContext() = this.synchronized {
      assert(started, "is server started?")
      handler = null
    }

    def start() = self.synchronized {
      started = true
      new Subscription {
        def unsubscribe() = self.synchronized {
          started = false
        }
      }
    }

    def emit(req: Request) = {
      val exchange = new DummyExchange(req)
      if (handler != null) handler(exchange)
      exchange
    }
  }

  class DummyServer(val port: Int) extends NodeScala {
    self =>
    val listeners = mutable.Map[String, DummyListener]()

    def createListener(relativePath: String) = {
      val l = new DummyListener(port, relativePath)
      listeners(relativePath) = l
      l
    }

    def emit(relativePath: String, req: Request) = this.synchronized {
      val l = listeners(relativePath)
      l.emit(req)
    }
  }

  test("Listener should serve the next request as a future") {
    val dummy = new DummyListener(8191, "/test")
    val subscription = dummy.start()

    def test(req: Request) {
      val f = dummy.nextRequest()
      dummy.emit(req)
      val (reqReturned, xchg) = Await.result(f, 1 second)

      assert(reqReturned == req)
    }

    test(immutable.Map("StrangeHeader" -> List("StrangeValue1")))
    test(immutable.Map("StrangeHeader" -> List("StrangeValue2")))

    subscription.unsubscribe()
  }

  test("Server should serve requests") {
    val dummy = new DummyServer(8191)
    val dummySubscription = dummy.start("/testDir") {
      request => for (kv <- request.iterator) yield (kv + "\n").toString
    }

    // wait until server is really installed
    Thread.sleep(500)

    def test(req: Request) {
      val webpage = dummy.emit("/testDir", req)
      val content = Await.result(webpage.loaded.future, 1 second)
      val expected = (for (kv <- req.iterator) yield (kv + "\n").toString).mkString
      assert(content == expected, s"'$content' vs. '$expected'")
    }

    test(immutable.Map("StrangeRequest" -> List("Does it work?")))
    test(immutable.Map("StrangeRequest" -> List("It works!")))
    test(immutable.Map("WorksForThree" -> List("Always works. Trust me.")))

    dummySubscription.unsubscribe()
  }
}
