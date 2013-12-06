package week4

import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global
import scala.util._

trait Observable[T] {

  self =>

  def subscribe(observer: Observer[T]): Subscription

  def subscribe(
    _onNext: T => Unit = (t: T) => {},
    _onError: Throwable => Unit = (e: Throwable) => {},
    _onCompleted: () => Unit = () => {}): Subscription = subscribe(new Observer[T] {

    def onNext(value: T): Unit = _onNext(value)

    def onError(error: Throwable): Unit = _onError(error)

    def onCompleted(): Unit = _onCompleted()
  })

  def filter(p: T => Boolean): Observable[T] = Observable[T] { observer: Observer[T] =>
    subscribe(
      (value: T) => if (p(value)) observer.onNext(value),
      (error: Throwable) => observer.onError(error),
      () => observer.onCompleted()
    )
  }

  def map[U](f: T => U): Observable[U] = Observable[U] { observer: Observer[U] =>
    subscribe(
      (value: T) => observer.onNext(f(value)),
      (error: Throwable) => observer.onError(error),
      () => observer.onCompleted()
    )
  }

  def buffer(slicesOf: Int, shiftedBy: Int): Observable[Seq[T]] = new Observable[Seq[T]] {

    def subscribe(observer: Observer[Seq[T]]): Subscription = {

      if (slicesOf <= 0 || shiftedBy <= 0) {
        throw new IllegalArgumentException
      }

      self.subscribe(new Observer[T] {

        val buffer: Array[Any] = Array.ofDim[Any](slicesOf)

        var index = 0

        def onError(error: Throwable): Unit = observer.onError(error)

        def onCompleted(): Unit = observer.onCompleted()

        def onNext(value: T): Unit = {
          if (index >= 0) {
            buffer(index) = value
          }
          index += 1
          if (index == buffer.length) {
            observer.onNext(buffer.toSeq.asInstanceOf[Seq[T]])
            if (shiftedBy < buffer.length) {
              Array.copy(buffer, shiftedBy, buffer, 0, buffer.length - shiftedBy)
            }
            index -= shiftedBy
          }
        }
      })
    }
  }

  def startWith(xs: T*): Observable[T] = Observable(observer => {
    xs foreach observer.onNext
    subscribe(observer)
  })
}

trait Observer[T] {

  def onNext(value: T): Unit

  def onError(error: Throwable): Unit

  def onCompleted(): Unit
}

trait Subscription {

  def unsubscribe(): Unit
}

/**
 *
 * @author Bogdan Dumitriu
 */
object Observable {

  def apply[T](s: Observer[T] => Subscription): Observable[T] = new Observable[T] {

    def subscribe(observer: Observer[T]): Subscription = s(observer)
  }

  def never(): Observable[Nothing] = Observable[Nothing] { observer: Observer[Nothing] =>
    Subscription {}
  }

  def apply[T](error: Throwable): Observable[T] = Observable(observer => {
    observer.onError(error)
    Subscription {}
  })

  def interval(duration: Duration): Observable[Long] = new Observable[Long] {
    def subscribe(observer: Observer[Long]): Subscription = {
      val p = Promise[Unit]()
      loop(observer, p, 0)
      new Subscription.Default(p)
    }

    private def loop(observer: Observer[Long], promise: Promise[Unit], nextValue: Long): Unit = {
      Future {
        blocking {
          Thread.sleep(duration.toMillis)
        }
      } onComplete { result =>
        if (!promise.isCompleted) {
          result match {
            case Failure(t) => observer.onError(t)
            case Success(_) => observer.onNext(nextValue)
          }
          loop(observer, promise, nextValue + 1)
        }
      }
    }
  }

  def range(from: Long, to: Long) = ???
}

object Subscription {

  def apply(unit: Unit): Subscription = new Subscription {
    def unsubscribe(): Unit = ()
  }

  class Default(private val p: Promise[Unit]) extends Subscription {

    def unsubscribe(): Unit = p.trySuccess(())
  }
}
