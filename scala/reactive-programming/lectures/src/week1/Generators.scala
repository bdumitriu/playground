package week1

class Generators {

  trait Generator[+T] {
    self =>

    def generate: T

    def map[S](f: T => S): Generator[S] = new Generator[S] {
      def generate: S = f(self.generate)
    }

    def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
      def generate: S = f(self.generate).generate
    }

    def withFilter(f: T => Boolean): Generator[T] = new Generator[T] {
      def generate: T = self.generate
    }
  }

  val Integers = new Generator[Int] {
    val rand = new java.util.Random
    def generate = rand.nextInt()
  }

  val Booleans = for (x <- Integers) yield x > 0

  def Pairs[T, U](t: Generator[T], u: Generator[U]): Generator[(T, U)] = for {
    x <- t
    y <- u
  } yield (x, y)

  def single[T](x: T): Generator[T] = new Generator[T] {
    def generate = x
  }

  def choose(lo: Int, hi: Int): Generator[Int] = {
    for (x <- Integers) yield lo + x % (hi - lo)
  }

  def oneOf[T](xs: T*): Generator[T] = {
    for (idx <- choose(0, xs.length)) yield xs(idx)
  }

  def lists: Generator[List[Int]] = for {
    isEmpty <- Booleans
    list <- if (isEmpty) emptyLists else nonEmptyLists
  } yield list

  def emptyLists = single(Nil)

  def nonEmptyLists = for {
    head <- Integers
    tail <- lists
  } yield head :: tail

  trait Tree

  case class Inner(left: Tree, right: Tree) extends Tree
  case class Leaf(x: Int) extends Tree

  def trees: Generator[Tree] = for {
    isLeaf <- Booleans
    tree <- if (isLeaf) leaves else innerNodes
  } yield tree

  def leaves = for (x <- Integers) yield Leaf(x)

  def innerNodes = for {
    l <- trees
    r <- trees
  } yield Inner(l, r)
}
