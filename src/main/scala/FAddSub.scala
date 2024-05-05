import chisel3._
import chisel3.util._

class FloatingPoint extends Bundle {
  val sign = UInt(1.W)
  val exp = UInt(8.W)
  val significand = UInt(23.W)

  def isZero(): Bool = exp === 0.U && significand === 0.U
  def isInf(): Bool = exp === 255.U && significand === 0.U
  def isNaN(): Bool = exp === 255.U && significand =/= 0.U
}

class FAddSub extends Module {
  val io = IO(new Bundle {
    val a = Input(new FloatingPoint)
    val b = Input(new FloatingPoint)
    val op = Input(Bool())  // True for addition, false for subtraction
    val valid = Input(Bool())
    val ack = Input(Bool())
    val idle = Output(Bool())
    val out = Output(new FloatingPoint)
    val ready = Output(Bool())
  })

    val sWait :: sCheck :: sAlign :: sAddSubtract :: sNormalize :: sOverflow :: sFinish :: Nil = Enum(7)
    val state = RegInit(sWait)
    val signOutReg = RegInit(0.U(1.W))
    val expOutReg = RegInit(0.U(9.W))
    val significandOutReg =  RegInit(0.U(23.W))


    val alignedA =RegInit(0.U(49.W))
    val alignedB = RegInit(0.U(49.W))
    val alignedExp =  RegInit(0.U(9.W))
    val compAlignedA  = Wire(UInt(50.W))
    val compAlignedB = Wire(UInt(50.W))
    val addSubResultReg=RegInit(0.U(49.W))
    val addSubExpReg=RegInit(0.U(9.W))
    val addSubSignReg=RegInit(0.U(1.W))
  
    
    io.ready := false.B
    io.idle := false.B
    io.out.sign := signOutReg
    io.out.exp := expOutReg(7,0)
    io.out.significand := significandOutReg
    compAlignedA  := 0.U
    compAlignedB := 0.U

    switch(state) {
        is(sWait) {
            signOutReg := 0.U
            expOutReg := 0.U
            significandOutReg :=  0.U
            addSubResultReg := 0.U 
            addSubExpReg := 0.U
            addSubSignReg := 0.U
            alignedA := 0.U
            alignedB := 0.U
            alignedExp := 0.U
            compAlignedA  := 0.U
            compAlignedB := 0.U
          
            when(io.valid) {
                state := sCheck
                io.idle := false.B
            }
        }
        is(sCheck) {
            when(io.a.isNaN || io.b.isNaN) {
                // 若任一操作数为NaN，输出为NaN
                signOutReg := 0.U
                expOutReg := 255.U
                significandOutReg := "b00000000000000000000001".U // example for signaling NaN
                state := sFinish
             
            }.elsewhen((io.a.isInf && !io.b.isInf) || (io.b.isInf && !io.a.isInf)) {
                // 处理无穷大情况
                signOutReg := 0.U
                expOutReg := 255.U
                significandOutReg := "b00000000000000000000000".U
                state := sFinish
               
            }.elsewhen(io.a.isInf && io.b.isInf) {
                when(io.a.sign === io.b.sign) {
                // 同号无穷大相加或相减
                    signOutReg := io.a.sign
                    expOutReg := 255.U
                    significandOutReg := "b00000000000000000000000".U
                    state := sFinish
                  
                }.otherwise {
                // 异号无穷大
                    signOutReg := 0.U
                    expOutReg := 255.U
                    significandOutReg := "b00000000000000000000001".U // example for signaling NaN
                    state := sFinish
                  
                }
            }.elsewhen(io.a.isZero && io.b.isZero) {
                // 都是零
                signOutReg := io.a.sign & io.b.sign
                expOutReg := 0.U
                significandOutReg := 0.U
                state := sFinish
               
            }.otherwise {
                state := sAlign
            }
        }
        is(sAlign) {
            
            
            //原码 s1.****，或者s0.1***  49位  1，1，23，24
            // 1,1,23,24
            alignedA := io.a.sign ## 1.U(1.W) ## io.a.significand ## 0.U(24.W)
            alignedB := io.b.sign ## 1.U(1.W) ## io.b.significand ## 0.U(24.W)
            alignedExp := 0.U(1.W) ##io.a.exp //9位
            when(io.a.exp > io.b.exp) {
                // 如果a的指数大，将b的尾数右移
                alignedB := io.b.sign ##( (1.U(1.W) ## io.b.significand ## 0.U(24.W) ) >> (io.a.exp - io.b.exp))
                alignedExp := 0.U(1.W) ## io.a.exp
            }.elsewhen(io.a.exp < io.b.exp) {
               
                alignedA :=  io.a.sign ##( (1.U(1.W) ## io.a.significand ## 0.U(24.W) ) >> (io.b.exp - io.a.exp))
                alignedExp := 0.U(1.W) ## io.b.exp
            }

            // 存储对齐后的尾数和统一的指数
            state := sAddSubtract
        }

        is(sAddSubtract) {
               //转成变型补码50位  2，1，23，24
            compAlignedA := alignedA(48) ## alignedA(48)  ## (Mux(alignedA(48), ~alignedA(47,0) + 1.U, alignedA))(47,0)
            compAlignedB := alignedB(48) ## alignedB(48)  ## (Mux(alignedB(48), ~alignedB(47,0)  + 1.U, alignedB))(47,0)

            val resultTmp = Wire(SInt(50.W))
            when(io.op) { // 加法或减法
                resultTmp := compAlignedA.asSInt + compAlignedB.asSInt
            }.otherwise {
                resultTmp := compAlignedA.asSInt - compAlignedB.asSInt
            }
 
           
            //addSubResultReg是49位  1，1，23，24
            // 从补码转换回原码
            addSubResultReg := Mux(resultTmp(49), (~resultTmp + 1.S), resultTmp)(48,0)  // 去掉隐含的1
            addSubSignReg := resultTmp(49) // 结果的符号位
            addSubExpReg :=alignedExp
            state := sNormalize
        }

        is(sNormalize) {
           
            signOutReg := addSubSignReg
            val leadingZeros =  PriorityEncoder(Reverse(addSubResultReg))
             when(addSubResultReg(48) === 0.U && addSubResultReg=/= 0.U) {
                // 需要左移规格化
                significandOutReg := (addSubResultReg << (leadingZeros))(47, 25)
                expOutReg := addSubExpReg - leadingZeros + 1.U // 调整指数
                printf("<---\n")
            }.elsewhen(addSubResultReg(48)) {
                significandOutReg := addSubResultReg(47,25)
                expOutReg := addSubExpReg + 1.U
                 printf("---\n")
            }.otherwise{
                significandOutReg :=addSubSignReg
                expOutReg := 0.U
            }
            state := sOverflow
        }


        is(sOverflow) {
            
            when( expOutReg >= "b11111110".U ||  expOutReg <= "b00000001".U) {
                expOutReg := "b11111111".U // 溢出设置为无穷大
                significandOutReg := 0.U
            }
            state := sFinish
        }

        is(sFinish) {
           
            io.ready := true.B
            when(io.ack) {
                io.ready := false.B
                state := sWait
            }
        }

    }

}
object FAddSub extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new FAddSub(), Array("--target-dir","verilog"))
}