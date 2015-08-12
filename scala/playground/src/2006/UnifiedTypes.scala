object UnifiedTypes {
  def main(args: Array[String]): Unit = {
    val set = new scala.collection.mutable.HashSet[Any]
    set += "This is a string"
    set += 732
    set += 'c'
    set += true
    set += f
    val iter: Iterator[Any] = set.elements
    while (iter.hasNext) {
      Console.println(iter.next.toString())
    }
  }

  def f(): Unit = {
  }
}
