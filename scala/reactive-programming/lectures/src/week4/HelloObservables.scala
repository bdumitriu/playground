package week4

import scala.language.postfixOps
import scala.concurrent.duration._

/**
 *
 * @author Bogdan Dumitriu
 */
class HelloObservables {

  val ticks = Observable.interval(1 second)

  val events: Observable[Long] = ticks.filter { _ % 2 == 0 }

  val bufs: Observable[Seq[Long]] = events.buffer(2, 4)

  val s = bufs.subscribe(new Observer[Seq[Long]] {

    def onError(error: Throwable): Unit = error.printStackTrace()

    def onCompleted(): Unit = println("Done.")

    def onNext(value: Seq[Long]): Unit = println(value)
  })

  Thread.sleep(20000)

  s.unsubscribe()

  Thread.sleep(10000)
}
