class Point2D(xc: Int, yc: Int) {
  val x = xc
  val y = yc

  override def toString(): String = {
    "x = " + x + ", y = " + y
  }
}

class ColoredPoint2D(xc: Int, yc: Int, c: String)
  extends Point2D(xc, yc) {
  var color = c

  def setColor(newCol: String) = {
    color = newCol
  }

  override def toString(): String = {
    super.toString() + ", color = " + color
  }
}

class Point3D(xc: Int, yc: Int, zc: Int)
  extends Point2D(xc, yc) {
  val z = zc

  override def toString(): String = {
    super.toString() + ", z = " + z;
  }
}

/* No longer allowed...

class ColoredPoint3D(xc: Int, yc: Int, zc: Int, c: String)
extends Point3D(xc, yc, zc)
with ColoredPoint2D(xc, yc, c);

*/

object Points {
  def main(args: Array[String]): Unit = {
    val p1 = new Point2D(1, 2)
    val p2 = new ColoredPoint2D(2, 3, "red")
    val p3 = new Point3D(3, 4, 5)
//    val p4 = new ColoredPoint3D(5, 6, 7, "black")

    Console.println(p1)
    Console.println(p2)
    Console.println(p3)
//    Console.println(p4)
  }
}

