import week1._

val f: Function[String, String] = { case "ping" => "pong" }

f("ping")
//f("pong")

val g: PartialFunction[String, String] = { case "ping" => "pong" }
g.isDefinedAt("ping")
g.isDefinedAt("pong")

val data = List((1, ("a", "A")), (2, ("b", "B")), (3, ("c", "C")))

for {
  tuple <- data
  (x, y) = tuple
  (y1, y2) = y
  if y1 == "a"
} yield y2

trait Generator[+T] {
  def generate: T
}

val integers = new Generator[Int] {
  val rand = new java.util.Random
  def generate = rand.nextInt()
}

val booleans = new Generator[Boolean] {
  def generate = integers.generate > 0
}

val pairs = new Generator[(Int, Int)] {
  def generate = (integers.generate, integers.generate)
}
object gen extends Generators
gen.trees.generate
