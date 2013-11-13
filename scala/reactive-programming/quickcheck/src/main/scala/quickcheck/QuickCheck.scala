package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("gen2") = forAll { (a: Int, b: Int) =>
    findMin(insert(b, insert(a, empty))) == math.min(a, b)
  }

  property("gen3") = forAll { (a: Int) =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  property("gen4") = forAll { (h: H) =>
    isSorted(heapToList(h))
  }

  def isSorted(l: List[A]): Boolean =
    l.isEmpty || (l, l.tail).zipped.forall(ord.lteq(_, _))

  def heapToList(h: H): List[A] =
    if (isEmpty(h)) Nil else findMin(h) :: heapToList(deleteMin(h))

  property("gen5") = forAll { (h1: H, h2: H) =>
    val h = meld(h1, h2)
    if (isEmpty(h1) && isEmpty(h2))
      true
    else if (isEmpty(h1))
      findMin(h) == findMin(h2)
    else if (isEmpty(h2))
      findMin(h) == findMin(h1)
    else
      findMin(h) == math.min(findMin(h1), findMin(h2))
  }

  property("gen6") = forAll { (h: H) =>
    val m = if (isEmpty(h)) Int.MinValue else findMin(h)
    m == Int.MinValue || m == findMin(deleteMin(insert(m - 1, h)))
  }

  property("gen7") = forAll { (h1: H, h2: H) =>
    compareMeldWithOriginal(h1, h2, meld(h1, h2))
  }

  def compareMeldWithOriginal(h1: H, h2: H, hh: H): Boolean =
    if (isEmpty(h1))
      isEmpty(h2) && isEmpty(hh) || !isEmpty(h2) && !isEmpty(hh) && findMin(h2) == findMin(hh) && compareMeldWithOriginal(h1, deleteMin(h2), deleteMin(hh))
    else if (isEmpty(h2))
      isEmpty(h1) && isEmpty(hh) || !isEmpty(h1) && !isEmpty(hh) && findMin(h1) == findMin(hh) && compareMeldWithOriginal(deleteMin(h1), h2, deleteMin(hh))
    else {
      val h1Min = findMin(h1)
      val h2Min = findMin(h2)
      if (ord.lteq(h1Min, h2Min))
        findMin(h1) == findMin(hh) && compareMeldWithOriginal(deleteMin(h1), h2, deleteMin(hh))
      else
        findMin(h2) == findMin(hh) && compareMeldWithOriginal(h1, deleteMin(h2), deleteMin(hh))
    }

  lazy val genMap: Gen[Map[Int,Int]] = for {
    k <- arbitrary[Int]
    v <- arbitrary[Int]
    m <- oneOf(value(Map.empty[Int,Int]), genMap)
  } yield m.updated(k, v)

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[Int]
    h <- oneOf(value(empty), for (sh <- genHeap) yield insert(a, sh))
    //h <- frequency((1, value(empty)), (99, for (sh <- genHeap) yield insert(a, sh)))
  } yield h

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)
}
