object References {
  class Reference[a] {
    private var contents: a = _

    def set(value: a): Unit = { contents = value }
    def get: a = contents
  }

  def main(args: Array[String]): unit = {
    val cell = new Reference[Int]
    cell.set(13)
    Console.println("Reference contains the half of " + (cell.get * 2))
  }
}
