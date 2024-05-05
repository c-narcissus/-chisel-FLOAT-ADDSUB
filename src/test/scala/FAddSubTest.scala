import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class FAddSubTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "FAddSub"
  it should "case1" in {
    test(new FAddSub) { c =>
      c.io.valid.poke(true.B)
      c.io.op.poke(true.B) 
      c.io.a.sign.poke(0.U) 
      c.io.a.exp.poke("b10000000".U) 
      c.io.a.significand.poke("b10000000000000000000000".U) // 3

      c.io.b.sign.poke(1.U) 
      c.io.b.exp.poke("b01111110".U) 
      c.io.b.significand.poke("b01000000000000000000000".U) //- 0.625

      while (!c.io.ready.peek().litToBoolean) {
        c.clock.step(1)
      }
      c.io.out.sign.expect(0.U)
      c.io.out.exp.expect("b10000000".U) 
      c.io.out.significand.expect("b00110000000000000000000".U)  //2.375
            c.clock.step(1)
      c.io.ack.poke(true.B)
      c.clock.step(1)
      c.io.ack.poke(false.B)
    }
  }
    it should "case2" in {
    test(new FAddSub) { c =>
      c.io.valid.poke(true.B)
      c.io.op.poke(false.B) // sub operation
      c.io.a.sign.poke(0.U) 
      c.io.a.exp.poke("b10000000".U) 
      c.io.a.significand.poke("b10000000000000000000000".U) // 3

      c.io.b.sign.poke(0.U) 
      c.io.b.exp.poke("b01111110".U) 
      c.io.b.significand.poke("b01000000000000000000000".U) // 0.625

      while (!c.io.ready.peek().litToBoolean) {
        c.clock.step(1)
      }
      c.io.out.sign.expect(0.U)
      c.io.out.exp.expect("b10000000".U) 
      c.io.out.significand.expect("b00110000000000000000000".U)  //2.375
            c.clock.step(1)
      c.io.ack.poke(true.B)
      c.clock.step(1)
      c.io.ack.poke(false.B)
    }
  }

      it should "case3" in {
    test(new FAddSub) { c =>
      c.io.valid.poke(true.B)
      c.io.op.poke(false.B) // sub operation
      c.io.a.sign.poke(0.U) 
      c.io.a.exp.poke("b01111110".U) 
      c.io.a.significand.poke("b01000000000000000000000".U) // 0.625

      c.io.b.sign.poke(0.U) 
      c.io.b.exp.poke("b10000000".U) 
      c.io.b.significand.poke("b10000000000000000000000".U) // 3

      while (!c.io.ready.peek().litToBoolean) {
        c.clock.step(1)
      }
      c.io.out.sign.expect(1.U)
      c.io.out.exp.expect("b10000000".U) 
      c.io.out.significand.expect("b00110000000000000000000".U)  //-2.375
            c.clock.step(1)
      c.io.ack.poke(true.B)
      c.clock.step(1)
      c.io.ack.poke(false.B)
    }
  }

       it should "case4" in {
    test(new FAddSub) { c =>
      c.io.valid.poke(true.B)
      c.io.op.poke(false.B) // sub operation
      c.io.a.sign.poke(0.U) 
      c.io.a.exp.poke("b01111111".U) 
      c.io.a.significand.poke("b00100000000000000000000".U) // 1.125

      c.io.b.sign.poke(0.U) 
      c.io.b.exp.poke("b01111111".U) 
      c.io.b.significand.poke("b00001000000000000000000".U) // 1.03125

      while (!c.io.ready.peek().litToBoolean) {
        c.clock.step(1)
      }
      c.io.out.sign.expect(0.U)
      c.io.out.exp.expect("b01111011".U) 
      c.io.out.significand.expect("b10000000000000000000000".U)  //0.09375
            c.clock.step(1)
      c.io.ack.poke(true.B)
      c.clock.step(1)
      c.io.ack.poke(false.B)
    }
  }
        it should "case5" in {
    test(new FAddSub) { c =>
      c.io.valid.poke(true.B)
      c.io.op.poke(false.B) // sub operation
      c.io.a.sign.poke(1.U) 
      c.io.a.exp.poke("b01111111".U) 
      c.io.a.significand.poke("b00100000000000000000000".U) // -1.125

      c.io.b.sign.poke(1.U) 
      c.io.b.exp.poke("b01111111".U) 
      c.io.b.significand.poke("b00001000000000000000000".U) // -.03125

      while (!c.io.ready.peek().litToBoolean) {
        c.clock.step(1)
      }
      c.io.out.sign.expect(1.U)
      c.io.out.exp.expect("b01111011".U) 
      c.io.out.significand.expect("b10000000000000000000000".U)  //-0.09375
            c.clock.step(1)
      c.io.ack.poke(true.B)
      c.clock.step(1)
      c.io.ack.poke(false.B)
    }
  }

        it should "case6" in {
    test(new FAddSub) { c =>
      c.io.valid.poke(true.B)
      c.io.op.poke(true.B) // sub operation
      c.io.a.sign.poke(1.U) 
      c.io.a.exp.poke("b01111111".U) 
      c.io.a.significand.poke("b00100000000000000000000".U) // -1.125

      c.io.b.sign.poke(0.U) 
      c.io.b.exp.poke("b01111111".U) 
      c.io.b.significand.poke("b00001000000000000000000".U) // .03125

      while (!c.io.ready.peek().litToBoolean) {
        c.clock.step(1)
      }
      c.io.out.sign.expect(1.U)
      c.io.out.exp.expect("b01111011".U) 
      c.io.out.significand.expect("b10000000000000000000000".U)  //-0.09375
            c.clock.step(1)
      c.io.ack.poke(true.B)
      c.clock.step(1)
      c.io.ack.poke(false.B)
    }
  }
  it should "case7" in {
    test(new FAddSub) { c =>
      c.io.valid.poke(true.B)
      c.io.a.sign.poke(0.U)
      c.io.a.exp.poke("b11111111".U) // NaN
      c.io.a.significand.poke("b00000000000000000000001".U)

      c.io.b.sign.poke(0.U)
      c.io.b.exp.poke("b01111111".U) // Normal number
      c.io.b.significand.poke("b00000000000000000000000".U)

      while (!c.io.ready.peek().litToBoolean) {
        c.clock.step(1)
      }

      c.io.out.sign.expect(0.U)
      c.io.out.exp.expect("b11111111".U) // Should be NaN
      c.io.out.significand.expect("b00000000000000000000001".U)
            c.clock.step(1)
      c.io.ack.poke(true.B)
      c.clock.step(1)
      c.io.ack.poke(false.B)
    }
  }

  it should "case8" in {
    test(new FAddSub) { c =>
      c.io.valid.poke(true.B)
      c.io.a.sign.poke(0.U)
      c.io.a.exp.poke("b11111111".U) // Infinity
      c.io.a.significand.poke("b00000000000000000000000".U)

      c.io.b.sign.poke(0.U)
      c.io.b.exp.poke("b01111111".U)
      c.io.b.significand.poke("b00000000000000000000000".U)

      while (!c.io.ready.peek().litToBoolean) {
        c.clock.step(1)
      }

      c.io.out.sign.expect(0.U)
      c.io.out.exp.expect("b11111111".U) // Should still be infinity
      c.io.out.significand.expect("b00000000000000000000000".U)
            c.clock.step(1)
      c.io.ack.poke(true.B)
      c.clock.step(1)
      c.io.ack.poke(false.B)
    }
  }

  it should "case9" in {
    test(new FAddSub) { c =>
      c.io.valid.poke(true.B)
      c.io.op.poke(true.B) // Addition operation
      c.io.a.sign.poke(0.U)
      c.io.a.exp.poke("b01111111".U)
      c.io.a.significand.poke("b01000000000000000000000".U) // 1.25

      c.io.b.sign.poke(0.U)
      c.io.b.exp.poke("b01111111".U)
      c.io.b.significand.poke("b00100000000000000000000".U) // 1.125

      while (!c.io.ready.peek().litToBoolean) {
        c.clock.step(1)
      }

      c.io.out.sign.expect(0.U)
      c.io.out.exp.expect("b10000000".U) // Adjusted exponent
      c.io.out.significand.expect("b00110000000000000000000".U) // 2.375
            c.clock.step(1)
      c.io.ack.poke(true.B)
      c.clock.step(1)
      c.io.ack.poke(false.B)
    }
  }

  it should "case10" in {
    test(new FAddSub) { c =>
      c.io.valid.poke(true.B)
      c.io.op.poke(false.B) // Subtraction operation
      c.io.a.sign.poke(0.U)
      c.io.a.exp.poke("b01111111".U)
      c.io.a.significand.poke("b10000000000000000000000".U) // 1.5

      c.io.b.sign.poke(1.U) // Negative
      c.io.b.exp.poke("b01111111".U)
      c.io.b.significand.poke("b00000000000000000000000".U) // -1.0

      while (!c.io.ready.peek().litToBoolean) {
        c.clock.step(1)
      }

      c.io.out.sign.expect(0.U) // Result should be positive
      c.io.out.exp.expect("b10000000".U) // Adjusted exponent
      c.io.out.significand.expect("b01000000000000000000000".U) // 2.5 as result
            c.clock.step(1)
      c.io.ack.poke(true.B)
      c.clock.step(1)
      c.io.ack.poke(false.B)
    }
  }


  it should "case11" in {
    test(new FAddSub) { c =>
      c.io.valid.poke(true.B)
      c.io.op.poke(true.B)
      c.io.a.sign.poke(0.U)
      c.io.a.exp.poke("b11111110".U)
      c.io.a.significand.poke("b11111111111111111111111".U) 

      c.io.b.sign.poke(0.U)
      c.io.b.exp.poke("b11111110".U)
      c.io.b.significand.poke("b11111111111111111111111".U) 

      while (!c.io.ready.peek().litToBoolean) {
        c.clock.step(1)
      }

      c.io.out.exp.expect("b11111111".U) 
      c.io.out.significand.expect("b00000000000000000000000".U) // Significand should be zero for infinity
            c.clock.step(1)
      c.io.ack.poke(true.B)
      c.clock.step(1)
      c.io.ack.poke(false.B)
    }
  }

  it should "case12" in {
    test(new FAddSub) { c =>
      c.io.valid.poke(true.B)
      c.io.op.poke(true.B)
      c.io.a.sign.poke(0.U) 
      c.io.a.exp.poke("b01111110".U) 
      c.io.a.significand.poke("b01000000000000000000000".U) // 0.625

      c.io.b.sign.poke(0.U) 
      c.io.b.exp.poke("b10000000".U) 
      c.io.b.significand.poke("b10000000000000000000000".U) // 3

      while (!c.io.ready.peek().litToBoolean) {
        c.clock.step(1)
      }
      c.io.out.sign.expect(0.U)
      c.io.out.exp.expect("b10000000".U) 
      c.io.out.significand.expect("b11010000000000000000000".U)  //3.625
            c.clock.step(1)
      c.io.ack.poke(true.B)
      c.clock.step(1)
      c.io.ack.poke(false.B)
    }
  }

  it should "case13" in {
    test(new FAddSub) { c =>
      c.io.valid.poke(true.B)
      c.io.op.poke(true.B) 
      c.io.a.sign.poke(1.U) 
      c.io.a.exp.poke("b10000000".U) 
      c.io.a.significand.poke("b10000000000000000000000".U) // -3

      c.io.b.sign.poke(1.U) 
      c.io.b.exp.poke("b01111110".U) 
      c.io.b.significand.poke("b01000000000000000000000".U) //- 0.625

      while (!c.io.ready.peek().litToBoolean) {
        c.clock.step(1)
      }
      c.io.out.sign.expect(1.U)
      c.io.out.exp.expect("b10000000".U) 
      c.io.out.significand.expect("b11010000000000000000000".U)  //-3.625
            c.clock.step(1)
      c.io.ack.poke(true.B)
      c.clock.step(1)
      c.io.ack.poke(false.B)
    }
  }

  
}
