type t = {
  mutable i_register: int;
  v_registers: Registers.t;
  mutable pc: int;
  mutable sp : int;
  mutable stack : int array;
  mutable delay_timer: int;
  mutable sound_timer: int;
}

let create () = {
  i_register = 0;
  v_registers = Registers.create ();
  pc = 0x200;
  sp = 0;
  stack = Array.make 16 0;
  delay_timer = 0;
  sound_timer = 0;
}

let execute_0x0_opcode cpu display instruction =
  match instruction with
  | 0x00E0 ->
    Display.clear display;
    cpu.pc <- cpu.pc + 2;
  | _ -> ()

let execute_0x1_opcode cpu instruction =
  cpu.pc <- (instruction land 0x0FFF);
  ()

let execute_0x6xkk_opcode cpu instruction =
  let x = (instruction land 0x0F00) lsr 8 in
  let kk = (instruction land 0x00FF) in
  Registers.set cpu.v_registers x kk;
  cpu.pc <- cpu.pc + 2;
  ()

let execute_0x7xkk_opcode cpu instruction =
  let x = (instruction land 0x0F00) lsr 8 in
  let kk = (instruction land 0x00FF) in
  Registers.add_value cpu.v_registers x kk;
  cpu.pc <- cpu.pc + 2;
  ()

let execute_0xAnnn_opcode cpu instruction =
  cpu.i_register <- (instruction land 0x0FFF);
  cpu.pc <- cpu.pc + 2;
  ()

  let execute_0xDxyn_opcode cpu memory display instruction =
    let vx = (instruction land 0x0F00) lsr 8 in
    let vy = (instruction land 0x00F0) lsr 4 in
    let n = instruction land 0x000F in
    let x_pos = Registers.get cpu.v_registers vx in
    let y_pos = Registers.get cpu.v_registers vy in
  
    Registers.set cpu.v_registers 0xF 0; (* Reset collision flag *)
  
    for yline = 0 to n - 1 do
      if cpu.i_register + yline >= 0 && cpu.i_register + yline < 4096 then
        let sprite_byte = Memory.get_byte memory (cpu.i_register + yline) in
        for xline = 0 to 7 do
          let sprite_pixel = (sprite_byte land (0x80 lsr xline)) <> 0 in
          let screen_x = (x_pos + xline) mod 64 in
          let screen_y = (y_pos + yline) mod 32 in
          let screen_pixel = Display.cell_at display screen_y screen_x in
  
          if sprite_pixel then
            if screen_pixel then
              Registers.set cpu.v_registers 0xF 1;
            Display.set_cell_at display screen_y screen_x (not screen_pixel)
        done
      else
        (* Handle out-of-bounds memory access *)
        ()
    done;
  
    cpu.pc <- cpu.pc + 2;
    ()
  

let execute_opcode cpu memory display instruction = 
  match (instruction land 0xF000) with
  | 0x0000 ->
    execute_0x0_opcode cpu display instruction;
  | 0x1000 ->
    execute_0x1_opcode cpu instruction;
  | 0x6000 ->
    execute_0x6xkk_opcode cpu instruction;
  | 0x7000 ->
    execute_0x7xkk_opcode cpu instruction;
  | 0xA000 ->
    execute_0xAnnn_opcode cpu instruction;
  | 0xD000 ->
    execute_0xDxyn_opcode cpu memory display instruction;
  | _ ->
    cpu.pc <- cpu.pc + 2;
    ()


let step cpu (memory: Memory.t) (display: Display.t) = 
  let instruction = Memory.read_at_address memory cpu.pc in
  execute_opcode cpu memory display instruction;