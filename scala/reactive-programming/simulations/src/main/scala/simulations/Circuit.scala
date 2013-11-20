package simulations

import common._

class Wire {
  private var sigVal = false
  private var actions: List[Simulator#Action] = List()

  def getSignal: Boolean = sigVal
  
  def setSignal(s: Boolean) {
    if (s != sigVal) {
      sigVal = s
      actions.foreach(action => action())
    }
  }

  def addAction(a: Simulator#Action) {
    actions = a :: actions
    a()
  }
}

abstract class CircuitSimulator extends Simulator {

  val InverterDelay: Int
  val AndGateDelay: Int
  val OrGateDelay: Int

  def probe(name: String, wire: Wire) {
    wire addAction {
      () => afterDelay(0) {
        println(
          "  " + currentTime + ": " + name + " -> " +  wire.getSignal)
      }
    }
  }

  def inverter(input: Wire, output: Wire) {
    def invertAction() {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) { output.setSignal(!inputSig) }
    }
    input addAction invertAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire) {
    def andAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) { output.setSignal(a1Sig & a2Sig) }
    }
    a1 addAction andAction
    a2 addAction andAction
  }

  def orGate(a1: Wire, a2: Wire, output: Wire) {
    def orAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(OrGateDelay) { output.setSignal(a1Sig | a2Sig) }
    }
    a1 addAction orAction
    a2 addAction orAction
  }

  def orGate2(a1: Wire, a2: Wire, output: Wire) {
    val a1Not, a2Not, outputNot = new Wire
    inverter(a1, a1Not)
    inverter(a2, a2Not)
    andGate(a1Not, a2Not, outputNot)
    inverter(outputNot, output)
  }

  def demux(in: Wire, c: List[Wire], out: List[Wire]): Unit = c match {
    case Nil if 1 == out.length => andGate(in, in, out(0))
    case control :: controls if math.pow(2, c.length).toInt == out.length => {
      val nrBits = math.pow(2, controls.length).toInt
      val demuxLow, demuxHigh = List.fill(nrBits) {new Wire}
      val controlNot = new Wire
      inverter(control, controlNot)
      demux(in, controls, demuxLow)
      demux(in, controls, demuxHigh)
      val outParts: (List[Wire], List[Wire]) = out.splitAt(nrBits)
      (demuxHigh, outParts._1).zipped foreach {andGate(control, _, _)}
      (demuxLow, outParts._2).zipped foreach {andGate(controlNot, _, _)}
    }
    case _ => throw new IllegalArgumentException("Length of `out' has to be 2 to the power of length of `c'.")
  }
}

object Circuit extends CircuitSimulator {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  def andGateExample {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }

  //
  // to complete with orGateExample and demuxExample...
  //
}

object CircuitMain extends App {
  // You can write tests either here, or better in the test class CircuitSuite.
  Circuit.andGateExample
}
