package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
  
  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 3")
  }

  //
  // to complete with tests for orGate, demux, ...
  //
  test("orGate example") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "or 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === true, "or 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "or 3")
  }

  test("orGate2 example") {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "or 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === true, "or 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "or 3")
  }

  test("demux 1:2") {
    val in, c0, out0, out1 = new Wire
    demux(in, List(c0), List(out0, out1))
    
    in.setSignal(true)
    c0.setSignal(true)
    run

    assert(out0.getSignal === false, "demux 1:2 1.1")
    assert(out1.getSignal === true, "demux 1:2 1.2")

    in.setSignal(true)
    c0.setSignal(false)
    run

    assert(out0.getSignal === true, "demux 1:2 2.1")
    assert(out1.getSignal === false, "demux 1:2 2.2")

    in.setSignal(false)
    c0.setSignal(true)
    run

    assert(out0.getSignal === false, "demux 1:2 1")
    assert(out1.getSignal === false, "demux 1:2 2")

    in.setSignal(false)
    c0.setSignal(false)
    run

    assert(out0.getSignal === false, "demux 1:2 1")
    assert(out1.getSignal === false, "demux 1:2 2")
  }

  test("demux 2:4") {
    val in, c0, c1, out0, out1, out2, out3 = new Wire
    demux(in, List(c0, c1), List(out0, out1, out2, out3))

    in.setSignal(true)
    c0.setSignal(true)
    run

    assert(out0.getSignal === false, "demux 2:4 1")
    assert(out1.getSignal === false, "demux 2:4 1")
    assert(out2.getSignal === true, "demux 2:4 1")
    assert(out3.getSignal === false, "demux 2:4 1")


    in.setSignal(false)
    c0.setSignal(true)
    run

    assert(out0.getSignal === false, "demux 2:4 2")
    assert(out1.getSignal === false, "demux 2:4 2")
    assert(out2.getSignal === false, "demux 2:4 2")
    assert(out3.getSignal === false, "demux 2:4 2")
  }

  test("demux 3:8") {
    val in, c0, c1, c2, out0, out1, out2, out3, out4, out5, out6, out7 = new Wire
    val outputs = List(out0, out1, out2, out3, out4, out5, out6, out7)
    demux(in, List(c0, c1, c2), outputs)

    in.setSignal(true)
    c1.setSignal(true)
    run

    assert(out0.getSignal === false, "demux 3:8 1")
    assert(out2.getSignal === true, "demux 3:8 1")
    assert(out7.getSignal === false, "demux 3:8 1")
  }
}
