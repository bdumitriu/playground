object CompoundTypes {

  trait Cloneable extends Object with java.lang.Cloneable {
    override def clone(): Cloneable = {
      super.clone();
      this;
    }
  }

  trait Resetable {
    def reset: Unit;
  }

  def cloneAndReset(obj: Cloneable with Resetable): Cloneable = {
    val cloned = obj.clone();
    obj.reset;
    cloned;
  }

  class MyTest(c: Char) extends Object with Cloneable with Resetable {
    var myc = c;

    override def clone(): Cloneable = {
      new MyTest(c).asInstanceOf[Cloneable];
    }

    def reset: Unit = {
      myc = 'a';
    }
  }

  def main(args: Array[String]) = {
    val x = new MyTest('c');
    val y = cloneAndReset(x).asInstanceOf[MyTest];
    Console.println("x.myc = " + x.myc);
    Console.println("y.myc = " + y.myc);
  }

}

