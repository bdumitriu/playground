object test {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  val f: Function[String, String] = { case "ping" => "pong" }
                                                  //> f  : String => String = <function1>
  
  f("ping")                                       //> res0: String = pong
  //f("pong")
  
  val g: PartialFunction[String, String] = { case "ping" => "pong" }
                                                  //> g  : PartialFunction[String,String] = <function1>
  
  g.isDefinedAt("ping")                           //> res1: Boolean = true
  g.isDefinedAt("pong")                           //> res2: Boolean = false
  
  val data = List((1, ("a", "A")), (2, ("b", "B")), (3, ("c", "C")))
                                                  //> data  : List[(Int, (String, String))] = List((1,(a,A)), (2,(b,B)), (3,(c,C))
                                                  //| )
  
  for {
    tuple <- data
    (x, y) = tuple
    (y1, y2) = y
    if (y1 == "a")
  } yield y2                                      //> res3: List[String] = List(A)

  trait Generator[+T] {
    def generate: T
  }
  
  val integers = new Generator[Int] {
    val rand = new java.util.Random
    def generate = rand.nextInt()
  }                                               //> integers  : test.Generator[Int]{val rand: java.util.Random} = test$$anonfun$
                                                  //| main$1$$anon$1@6d475479
  val booleans = new Generator[Boolean] {
    def generate = integers.generate > 0
  }                                               //> booleans  : test.Generator[Boolean] = test$$anonfun$main$1$$anon$2@23493578
  
  val pairs = new Generator[(Int, Int)] {
    def generate = (integers.generate, integers.generate)
  }                                               //> pairs  : test.Generator[(Int, Int)] = test$$anonfun$main$1$$anon$3@6c6c7d22
  
  trait ImprovedGenerator[+T] {
    self => // an "alias" for this

    def generate: T

    def map[S](f: T => S): ImprovedGenerator[S] = new ImprovedGenerator[S] {
      def generate: S = f(self.generate)
    }
    
    def flatMap[S](f: T => ImprovedGenerator[S]): ImprovedGenerator[S] = new ImprovedGenerator[S] {
      def generate: S = f(self.generate).generate
    }
    
    def withFilter(f: T => Boolean): ImprovedGenerator[T] = new ImprovedGenerator[T] {
      def generate: T = self.generate
    }
  }
  
  val improvedIntegers = new ImprovedGenerator[Int] {
    val rand = new java.util.Random
    def generate = rand.nextInt()
  }                                               //> improvedIntegers  : test.ImprovedGenerator[Int]{val rand: java.util.Random}
                                                  //|  = test$$anonfun$main$1$$anon$7@484adff7

  val improvedBooleans = for (x <- improvedIntegers) yield x > 0
                                                  //> improvedBooleans  : test.ImprovedGenerator[Boolean] = test$$anonfun$main$1$
                                                  //| ImprovedGenerator$1$$anon$4@387d3660

  def improvedPairs[T, U](t: ImprovedGenerator[T], u: ImprovedGenerator[U]): ImprovedGenerator[(T, U)] = for {
    x <- t
    y <- u
  } yield (x, y)                                  //> improvedPairs: [T, U](t: test.ImprovedGenerator[T], u: test.ImprovedGenerat
                                                  //| or[U])test.ImprovedGenerator[(T, U)]

  def single[T](x: T): ImprovedGenerator[T] = new ImprovedGenerator[T] {
    def generate = x
  }                                               //> single: [T](x: T)test.ImprovedGenerator[T]

  def choose(lo: Int, hi: Int): ImprovedGenerator[Int] = {
    for (x <- improvedIntegers) yield lo + x % (hi - lo)
  }                                               //> choose: (lo: Int, hi: Int)test.ImprovedGenerator[Int]
  
  def oneOf[T](xs: T*): ImprovedGenerator[T] = {
    for (idx <- choose(0, xs.length)) yield xs(idx)
  }                                               //> oneOf: [T](xs: T*)test.ImprovedGenerator[T]
  
  def lists: ImprovedGenerator[List[Int]] = for {
    isEmpty <- improvedBooleans
    list <- if (isEmpty) emptyLists else nonEmptyLists
  } yield list                                    //> lists: => test.ImprovedGenerator[List[Int]]
  
  def emptyLists = single(Nil)                    //> emptyLists: => test.ImprovedGenerator[scala.collection.immutable.Nil.type]
  
  def nonEmptyLists = for {
    head <- improvedIntegers
    tail <- lists
  } yield head :: tail                            //> nonEmptyLists: => test.ImprovedGenerator[List[Int]]

  trait Tree
  
  case class Inner(left: Tree, right: Tree) extends Tree
  
  case class Leaf(x: Int) extends Tree
  
  def trees: ImprovedGenerator[Tree] = for {
    isLeaf <- improvedBooleans
    tree <- if (isLeaf) leaves else innerNodes
  } yield tree                                    //> trees: => test.ImprovedGenerator[test.Tree]
  
  def leaves = for (x <- improvedIntegers) yield Leaf(x)
                                                  //> leaves: => test.ImprovedGenerator[test.Leaf]
  def innerNodes = for {
    l <- trees
    r <- trees
  } yield Inner(l, r)                             //> innerNodes: => test.ImprovedGenerator[test.Inner]

  trees.generate                                  //> res4: test.Tree = Inner(Leaf(-1161397816),Inner(Leaf(-1297420762),Leaf(-185
                                                  //| 6469771)))
}