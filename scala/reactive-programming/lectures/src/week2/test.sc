import week2._

class RepeatContinuation(command: => Unit) {
  def until(condition: => Boolean) {
    command
    if (!condition) {
      until(condition)
    }
  }
}

def repeat(command: => Unit): RepeatContinuation = new RepeatContinuation(command)

var x = 1

repeat {x = x + 1; println(x)} until (x == 10)
x

object sim extends Circuits with Parameters
import sim._

val in1, in2, sum, carry = new Wire
halfAdder(in1, in2, sum, carry)
probe("sum", sum)
probe("carry", carry)
in1 setSignal true
run()
in2 setSignal true
run()
in1 setSignal false
run()
