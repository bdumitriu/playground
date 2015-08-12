object MyStringViews {
  /*implicit*/ def view(str: String): Seq[Char] = new Seq[Char] {
    def length = str.length();
    def elements = Iterator.fromString("a");
    def apply(n: Int) = str.charAt(n);
    override def hashCode(): Int = str.hashCode();
    override def equals(y: Any): Boolean = str.equals(y);
    override def toString(): String = str;
  }
}

object ViewTest {
  import MyStringViews._;

  def printElements[T](seq: Seq[T]) = {
    val it = seq.elements;
    while (it.hasNext) {
      Console.println("'" + it.next + "' ");
    }
  }

  def main(args: Array[String]): Unit = {
    var i = 0;
    while (i < args.length) {
      printElements(args(i));
      i = i + 1;
    }
    Console.println;
  }
}

