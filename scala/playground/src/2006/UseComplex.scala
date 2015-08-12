class Complex(real: double, imaginary: double)
{
  def re() = real
  def im() = imaginary

  override def toString() =
    "" + re + (if (im < 0) "-" else "+") + im + "i"
}

class NewComplex(real: double, imaginary: double)
{
  def re = real
  def im = imaginary
}

object UseComplex
{
  def main(args: Array[String]): unit =
  {
    var c = new Complex(1.2, 3.4)
    var d = new NewComplex(1.2, 3.4)
    Console.println("real part: " + c.re())
    Console.println("imaginary part: " + d.im)
    Console.println(" c = " + c)
  }
}
