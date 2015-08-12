object ObjType {
  class C {
    protected var x = 0;
    def incr: this.type = { x = x + 1; this }
    def getx = x;
  }

  class D extends C {
    def decr: this.type = { x = x - 1; this }
  }

  def main(args: Array[String]): Unit = {
    val d: D = new D;
    d.incr.decr;
    Console.println(d.getx);
  }
}

