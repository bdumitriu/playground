package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
  
  test("andGate") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "false & false")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === false, "true & false")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "true & true")

    in1.setSignal(false)
    run

    assert(out.getSignal === false, "false & true")
  }

  test("orGate") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "false | false")

    in1.setSignal(true)
    run

    assert(out.getSignal === true, "true | false")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "true | true")

    in1.setSignal(false)
    run

    assert(out.getSignal === true, "false | true")
  }

  test("orGate2") {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "false | false")

    in1.setSignal(true)
    run

    assert(out.getSignal === true, "true | false")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "true | true")

    in1.setSignal(false)
    run

    assert(out.getSignal === true, "false | true")
  }

  test("demux 0") {
    val in = new Wire
    val control = List()
    val out = List(new Wire)
    demux(in, control, out)
    in.setSignal(false)
    run

    assert(out(0).getSignal === false)

    in.setSignal(true)
    run

    assert(out(0).getSignal === true)

    in.setSignal(false)
    run

    assert(out(0).getSignal === false)
  }

  test("demux 1") {
    val in = new Wire
    val control = List(new Wire)
    val out = List(new Wire, new Wire)
    demux(in, control, out)
    in.setSignal(false)
    control(0).setSignal(false)
    run

    assert(out(0).getSignal === false, "c = 0, in = 0, out(0)")
    assert(out(1).getSignal === false, "c = 0, in = 0, out(1)")

    control(0).setSignal(true)
    run

    assert(out(0).getSignal === false, "c = 1, in = 0, out(0)")
    assert(out(1).getSignal === false, "c = 1, in = 0, out(1)")

    in.setSignal(true)
    run

    assert(out(0).getSignal === true, "c = 1, in = 1, out(0)")
    assert(out(1).getSignal === false, "c = 1, in = 1, out(1)")

    control(0).setSignal(false)
    run

    assert(out(0).getSignal === false, "c = 0, in = 1, out(0)")
    assert(out(1).getSignal === true, "c = 0, in = 1, out(1)")
  }

  test("demux 2") {
    val in = new Wire
    val control = List(new Wire, new Wire)
    val out = List(new Wire, new Wire, new Wire, new Wire)
    demux(in, control, out)
    in.setSignal(false)
    control(0).setSignal(false)
    control(1).setSignal(false)
    run

    assert(out(0).getSignal === false, "c = 00, in = 0, out(0)")
    assert(out(1).getSignal === false, "c = 00, in = 0, out(1)")
    assert(out(2).getSignal === false, "c = 00, in = 0, out(2)")
    assert(out(3).getSignal === false, "c = 00, in = 0, out(3)")

    control(0).setSignal(true)
    run

    assert(out(0).getSignal === false, "c = 10, in = 0, out(0)")
    assert(out(1).getSignal === false, "c = 10, in = 0, out(1)")
    assert(out(2).getSignal === false, "c = 10, in = 0, out(2)")
    assert(out(3).getSignal === false, "c = 10, in = 0, out(3)")

    in.setSignal(true)
    run

    assert(out(0).getSignal === false, "c = 10, in = 1, out(0)")
    assert(out(1).getSignal === true, "c = 10, in = 1, out(1)")
    assert(out(2).getSignal === false, "c = 10, in = 1, out(2)")
    assert(out(3).getSignal === false, "c = 10, in = 1, out(3)")

    control(1).setSignal(true)
    run

    assert(out(0).getSignal === true, "c = 11, in = 1, out(0)")
    assert(out(1).getSignal === false, "c = 11, in = 1, out(1)")
    assert(out(2).getSignal === false, "c = 11, in = 1, out(2)")
    assert(out(3).getSignal === false, "c = 11, in = 1, out(3)")

    control(0).setSignal(false)
    run

    assert(out(0).getSignal === false, "c = 01, in = 1, out(0)")
    assert(out(1).getSignal === false, "c = 01, in = 1, out(1)")
    assert(out(2).getSignal === true, "c = 01, in = 1, out(2)")
    assert(out(3).getSignal === false, "c = 01, in = 1, out(3)")

    control(1).setSignal(false)
    run

    assert(out(0).getSignal === false, "c = 00, in = 1, out(0)")
    assert(out(1).getSignal === false, "c = 00, in = 1, out(1)")
    assert(out(2).getSignal === false, "c = 00, in = 1, out(2)")
    assert(out(3).getSignal === true, "c = 00, in = 1, out(3)")
  }

  test("demux 3") {
    val in = new Wire
    val control = List(new Wire, new Wire, new Wire)
    val out = List(new Wire, new Wire, new Wire, new Wire, new Wire, new Wire, new Wire, new Wire)
    demux(in, control, out)
    in.setSignal(false)
    control(0).setSignal(false)
    control(1).setSignal(false)
    control(2).setSignal(false)
    run

    assert(out(0).getSignal === false, "c = 000, in = 0, out(0)")
    assert(out(1).getSignal === false, "c = 000, in = 0, out(1)")
    assert(out(2).getSignal === false, "c = 000, in = 0, out(2)")
    assert(out(3).getSignal === false, "c = 000, in = 0, out(3)")
    assert(out(4).getSignal === false, "c = 000, in = 0, out(4)")
    assert(out(5).getSignal === false, "c = 000, in = 0, out(5)")
    assert(out(6).getSignal === false, "c = 000, in = 0, out(6)")
    assert(out(7).getSignal === false, "c = 000, in = 0, out(7)")

    in.setSignal(true)
    control(1).setSignal(true)
    run

    assert(out(0).getSignal === false, "c = 010, in = 1, out(0)")
    assert(out(1).getSignal === false, "c = 010, in = 1, out(1)")
    assert(out(2).getSignal === false, "c = 010, in = 1, out(2)")
    assert(out(3).getSignal === false, "c = 010, in = 1, out(3)")
    assert(out(4).getSignal === false, "c = 010, in = 1, out(4)")
    assert(out(5).getSignal === true, "c = 010, in = 1, out(5)")
    assert(out(6).getSignal === false, "c = 010, in = 1, out(6)")
    assert(out(7).getSignal === false, "c = 010, in = 1, out(7)")

    control(2).setSignal(true)
    run

    assert(out(0).getSignal === false, "c = 011, in = 1, out(0)")
    assert(out(1).getSignal === false, "c = 011, in = 1, out(1)")
    assert(out(2).getSignal === false, "c = 011, in = 1, out(2)")
    assert(out(3).getSignal === false, "c = 011, in = 1, out(3)")
    assert(out(4).getSignal === true, "c = 011, in = 1, out(4)")
    assert(out(5).getSignal === false, "c = 011, in = 1, out(5)")
    assert(out(6).getSignal === false, "c = 011, in = 1, out(6)")
    assert(out(7).getSignal === false, "c = 011, in = 1, out(7)")

    control(0).setSignal(true)
    control(1).setSignal(false)
    control(2).setSignal(false)
    run

    assert(out(0).getSignal === false, "c = 100, in = 1, out(0)")
    assert(out(1).getSignal === false, "c = 100, in = 1, out(1)")
    assert(out(2).getSignal === false, "c = 100, in = 1, out(2)")
    assert(out(3).getSignal === true, "c = 100, in = 1, out(3)")
    assert(out(4).getSignal === false, "c = 100, in = 1, out(4)")
    assert(out(5).getSignal === false, "c = 100, in = 1, out(5)")
    assert(out(6).getSignal === false, "c = 100, in = 1, out(6)")
    assert(out(7).getSignal === false, "c = 100, in = 1, out(7)")
  }

  test("demux err c Nil") {
    val in = new Wire
    val control = List()
    val out = List(new Wire, new Wire, new Wire)
    intercept[IllegalArgumentException] {
      demux(in, control, out)
    }
  }

  test("demux err c non Nil") {
    val in = new Wire
    val control = List(new Wire, new Wire)
    val out = List(new Wire, new Wire, new Wire)
    intercept[IllegalArgumentException] {
      demux(in, control, out)
    }
  }
}
