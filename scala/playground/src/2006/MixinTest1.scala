object MixinTest1 {
  trait Ord {
    def < (that: Any): boolean
    def <=(that: Any): boolean = (this < that) || (this == that)
    def > (that: Any): boolean = !(this <= that)
    def >=(that: Any): boolean = !(this < that)
  }

  class Date(y: int, m: int, d: int) extends Object with Ord {
    def year = y
    def month = m
    def day = d

    override def toString(): String = year + "-" + month + "-" + day

    override def equals(that: Any): boolean = {
      that.isInstanceOf[Date] && {
        val o = that.asInstanceOf[Date]
        o.day == day && o.month == month && o.year == year
      }
    }

    def <(that: Any): boolean = {
      if (!that.isInstanceOf[Date])
        error("cannot compare " + that + " and a Date")

      val o = that.asInstanceOf[Date]
      (year < o.year) || (year == o.year && (month < o.month || (month == o.month && day < o.day)))
    }
  }

  def main(args: Array[String]): unit = {
    var d1 = new Date(2006, 4, 23)
    var d2 = new Date(1981, 2, 2)
    var d3 = new java.util.Date()

    Console.println("" + d1 + " < " + d2 + " is " + (d1 < d2))
  }
}
