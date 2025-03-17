type t = {
  i_register: int;
  v_registers: Registers.t;
  pc: int;
  sp : int;
  stack : int array;
}

let create () = {
  i_register = 0;
  v_registers = Registers.create ();
  pc = 0x200;
  sp = 0;
  stack = Array.make 16 0;
}

let step cpu (memory: Memory.t) = 
  let instruction = Memory.read_at_address memory cpu.pc in
  Printf.printf "PC: 0x%04X - Fetched Instruction: 0x%04X\n" cpu.pc instruction;
  {
  i_register = cpu.i_register;
  v_registers = cpu.v_registers;
  pc = cpu.pc + 2;
  sp = cpu.sp;
  stack = Array.copy cpu.stack;
}