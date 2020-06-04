 package TutorialExamples
 
 // Example CPU 2 bit data bus, code Instruction 9 
 // code Instruction: bit 3 op and 6 bit reg index:  2bit dest, 2 bit source1 and 2 bit source2
 // This file is modify of Risc.scala TutorialExamples may 2020.
 // Set Instruction are: Arithmetic only add, control only jump and logic 3: and, or, xor. 
 // This example show the HSL with Chisel by used Scala Language 
 // (CH>)
 
 import Chisel._       // version 2.x
 
 class Risc extends Module {
   val io = new Bundle {           // module IO
     val isWr   = Bool(INPUT)      // Input signal Wr
     val wrAddr = UInt(INPUT, 8)   // Input Address
     val wrData = Bits(INPUT, 9)   // Input data bus
     val boot   = Bool(INPUT)      // Input reset
     val valid  = Bool(OUTPUT)     // validad out
     val out    = Bits(OUTPUT, 2)  // output Bus
   }
   val file = Mem(256, Bits(width = 2)) // reg 256 of 2 bit
   val code = Mem(256, Bits(width = 9)) // mem 256 of 10 bit
   val pc   = Reg(init=UInt(0, 8))      // Program Counter
   
   val add_op :: imm_op :: or_op::xor_op::jmp_op :: and_op::  Nil = Enum(Bits(), 6) // Only two instruction or1_op::
 
   val inst = code(pc)                           // decode of Instrucction
   val op   = inst(8,6)                          // 3 bit op
   val rci  = inst(5,4)                          // op rc 4 reg
   val rai  = inst( 3, 2)                        // op ra 4 reg
   val rbi  = inst( 1, 0)                        // op rb 4 reg
 
   val ra = Mux(rai === Bits(0), Bits(0), file(rai))  // 
   val rb = Mux(rbi === Bits(0), Bits(0), file(rbi))  //
   val rc = Wire(Bits(width = 2))                    // destino rc
 
   io.valid := Bool(false)                           // inicial reset
   io.out   := Bits(0)
   rc       := Bits(0)
 
   when (io.isWr) {                                 // op_write
     code(io.wrAddr) := io.wrData
   } .elsewhen (io.boot) {                          // reset
     pc := UInt(0)                                  // PC =0
   } .otherwise {
     switch(op) {
       is(add_op) { rc := ra + rb }                    // add rc ra rb
       is(imm_op) { rc := (rai << UInt(2)) | rbi }     // rc= rai | rbi
       is(or_op)  { rc := ra | rb }		       // rc= rai | rbi
       is(xor_op)  { rc := ra ^ rb }		       // rc= rai ^ rbi
       is(and_op)  { rc := ra & rb }		       // rc= rai & rbi
       //is(jmp_op)  { pc := pc+((rai << UInt(2)) | rbi)  } //+ UInt(0)} //((rai << UInt(2)) | rbi) } // rc= rai & rbi
     }
     io.out := rc                                      // rc result
     when (rci === UInt(3)) {
       io.valid := Bool(true)                          // valid out data
     } .otherwise {
       file(rci) := rc                                 // write register reg
     }
     when(op === jmp_op){
     	pc := pc + ((rai << UInt(2)) | rbi)          //UInt(3)                                increment pc
     } .otherwise{    
       pc := pc + UInt(1)                                // increment pc  
     }
       
      
   }
 }


 
 class RiscTests(c: Risc) extends Tester(c) {  		// UUT   Testbech
   def wr(addr: UInt, data: UInt)  = {
     poke(c.io.isWr,   1)
     poke(c.io.wrAddr, addr.litValue())
     poke(c.io.wrData, data.litValue())
     step(1)
   }
   def boot()  = {
     poke(c.io.isWr, 0)
     poke(c.io.boot, 1)
     step(1)
   }
   def tick()  = {
     poke(c.io.isWr, 0)
     poke(c.io.boot, 0)
     step(1)
   }
   def I (op: UInt, rc: Int, ra: Int, rb: Int) = 
     Cat(op, UInt(rc, 2), UInt(ra, 2), UInt(rb, 2)) //                  op  rd  rs1 rs2   hex
   val app  = Array(I(c.jmp_op,   1, 0, 2),        // pc <- pc +1      1 01 01  00  01  = x150
   		    I(c.xor_op,   2, 2, 2),       // rb <- rb ^ rb     0 11 10  10  10  = xEA
   		    I(c.imm_op,   2, 0, 1),       // ra <- 1           0 01 01  00  01  = x51		    
   		    //I(c.xor_op,   2, 2, 2),       // rb <- rb ^ rb     0 11 10  10  10  = xEA
   		    //I(c.xor_op,   2, 2, 2),       // rb <- rb ^ rb     0 11 10  10  10  = xEA
   		    //I(c.xor_op,   2, 2, 2),       // rb <- rb ^ rb     0 11 10  10  10  = xEA
   		    I(c.imm_op,   1, 0, 2),       // ra <- 1           0 01 01  00  01  = x52		    
                    //I(c.xor_op,   2, 2, 2),       // rb <- rb ^ rb     0 11 10  10  10  = xEA
                    //I(c.xor_op,   2, 2, 2),       // rb <- rb ^ rb     0 11 10  10  10  = xEA 
                    I(c.and_op,   1, 1, 1),       // ra <- ra & ra     1 00 01  01  01  = x115
                    I(c.add_op,   1, 2, 1),       // ra <- rb + ra     0 00 01  10  01  = x19
                    I(c.add_op,   3, 1, 0))       // rh <- ra          0 00 11  01  00  = x34
                    
   wr(UInt(0), Bits(0)) // skip reset
   for (addr <- 0 until app.length) 
     wr(UInt(addr), app(addr))
   boot()
   var k = 0
   do {
     tick(); k += 1
   } while (peek(c.io.valid) == 0 && k < 11)
   expect(k < 20, "TIME LIMIT")
   expect(c.io.out, 3)
}