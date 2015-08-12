/**
 *
 * @author Bogdan Dumitriu
 */

class Rational(n: Int, d: Int) {

  require(n != 0)

  private val g = gcd(n.abs, d.abs)
  val numer: Int = n / g;
  val denom: Int = d / g;

  def this(n: Int) = this(n, 1)

  override def toString = {
    numer + "/" + denom
  }

  def + (that: Rational): Rational = {
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )
  }

  def + (i: Int): Rational = {
    this + new Rational(i, 1)
  }

  def - (that: Rational): Rational = {
    new Rational(
      numer * that.denom - that.numer * denom,
      denom * that.denom
    )
  }

  def - (i: Int): Rational = {
    this - new Rational(i, 1)
  }

  def * (that: Rational): Rational = {
    new Rational(numer * that.numer, denom * that.denom)
  }

  def * (i: Int): Rational = {
    this * new Rational(i, 1)
  }

  def / (that: Rational): Rational = {
    new Rational(numer * that.denom, denom * that.numer)
  }

  def / (i: Int): Rational = {
    this / new Rational(i, 1)
  }

  private def gcd(a: Int, b: Int): Int = {
    if (b == 0) a else gcd(b, a % b)
  }
}
