object AnonymousFunctions {

  def main(args: Array[String]): Unit = {
    def app(f: Int => String, v: Int) = f(v);
    val decorator = new Decorator("[", "]");
    Console.println(app(decorator.layout, 7));
  }

  def test1(): Unit = {
    val a = Array(1,2,3,4,5,6,7,8,9);
    val f = x: Int => x + 1;
    val b = a.map(f);
    //b.map[Int](x: Int => {Console.println(x); x});
    for (val x <- b)
      Console.println(x);
  }

  class Decorator(left: String, right: String) {
    def layout[A](x: A) = {
      left + x.toString() + right;
    }
  }

}

