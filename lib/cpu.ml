type t = {
  mutable i_register: int;
  v_registers: Registers.t;
  mutable pc: int;
  mutable sp : int;
  mutable stack : int Stack.t;
  mutable delay_timer: int;
  mutable sound_timer: int;
}

let create () = {
  i_register = 0;
  v_registers = Registers.create ();
  pc = 0x200;
  sp = 0;
  stack = Stack.create ();
  delay_timer = 0;
  sound_timer = 0;
}

let execute_0x0_opcode cpu display instruction =
  match instruction with
  | 0x00E0 ->
    Display.clear display;
    cpu.pc <- cpu.pc + 2;
  | 0x00EE ->
    let proc_pointer = Stack.pop cpu.stack in
    cpu.pc <- proc_pointer; 
    cpu.sp <- cpu.sp - 1;
  | _ -> ()

let execute_0x1nnn_opcode cpu instruction =
  cpu.pc <- (instruction land 0x0FFF);
  ()

let execute_0x2nnn_opcode cpu instruction =
  let nnn = (instruction land 0x0FFF) in
  cpu.pc <- cpu.pc + 2;
  Stack.push cpu.pc cpu.stack;
  cpu.pc <- nnn;
  ()

let execute_0x3xkk_opcode cpu instruction =
  let x = (instruction land 0x0F00) lsr 8 in
  let kk = (instruction land 0x00FF) in
  if Registers.contains cpu.v_registers x kk then
    cpu.pc <- cpu.pc + 4
  else
    cpu.pc <- cpu.pc + 2

let execute_0x4xkk_opcode cpu instruction =
  let x = (instruction land 0x0F00) lsr 8 in
  let kk = (instruction land 0x00FF) in
  if not (Registers.contains cpu.v_registers x kk) then
    cpu.pc <- cpu.pc + 4
  else
    cpu.pc <- cpu.pc + 2

let execute_0x5xy0_opcode cpu instruction =
  let x = (instruction land 0x0F00) lsr 8 in
  let y = (instruction land 0x00F0) lsr 4 in
  if Registers.are_equal cpu.v_registers x y then
    cpu.pc <- cpu.pc + 4
  else
    cpu.pc <- cpu.pc + 2

let execute_0x6xkk_opcode cpu instruction =
  let x = (instruction land 0x0F00) lsr 8 in
  let kk = (instruction land 0x00FF) in
  Registers.set cpu.v_registers x kk;
  cpu.pc <- cpu.pc + 2;
  ()

let execute_0x7xkk_opcode cpu instruction =
  let x = (instruction land 0x0F00) lsr 8 in
  let kk = (instruction land 0x00FF) in
  let vx = Registers.get cpu.v_registers x in
  Registers.set cpu.v_registers x ((vx + kk) land 0xFF);
  cpu.pc <- cpu.pc + 2;
  ()

let execute_0x8_opcode cpu instruction =
  let x = (instruction land 0x0F00) lsr 8 in
  let y = (instruction land 0x00F0) lsr 4 in
  match (instruction land 0x000F) with
  | 0x0000 ->
    let vy = Registers.get cpu.v_registers y in
    Registers.set cpu.v_registers x vy;
  | 0x0001 ->
    let vy = Registers.get cpu.v_registers y in
    let vx = Registers.get cpu.v_registers x in
    Registers.set cpu.v_registers x (vx lor vy);
  | 0x0002 ->
    let vy = Registers.get cpu.v_registers y in
    let vx = Registers.get cpu.v_registers x in
    Registers.set cpu.v_registers x (vx land vy);
  | 0x0003 ->
    let vy = Registers.get cpu.v_registers y in
    let vx = Registers.get cpu.v_registers x in
    Registers.set cpu.v_registers x (vx lxor vy);
  | 0x0004 ->
    let vx = Registers.get cpu.v_registers x in
    let vy = Registers.get cpu.v_registers y in
    let sum_vx_vy = vx + vy in
    Registers.set cpu.v_registers x (sum_vx_vy land 0xFF);
    Registers.set cpu.v_registers 0xF ((sum_vx_vy lsr 8) land 1);
  | 0x0005 ->
    let vy = Registers.get cpu.v_registers y in
    let vx = Registers.get cpu.v_registers x in
    let diff = vx - vy in
    Registers.set cpu.v_registers x (diff land 0xFF);
    Registers.set cpu.v_registers 0xF (if vx >= vy then 1 else 0);
  | 0x0006 ->
    let vx = Registers.get cpu.v_registers x in
    let ls_bit = vx land 0x1 in
    let new_vx = vx lsr 1 in
    Registers.set cpu.v_registers x new_vx;
    Registers.set cpu.v_registers 0xF ls_bit;
  | 0x0007 ->
    let vy = Registers.get cpu.v_registers y in
    let vx = Registers.get cpu.v_registers x in
    let diff = vy - vx in
    Registers.set cpu.v_registers x (diff land 0xFF);
    Registers.set cpu.v_registers 0xF (if vy >= vx then 1 else 0);
  | 0x000E ->
    let vx = Registers.get cpu.v_registers x in
    Registers.set cpu.v_registers x ((vx lsl 1) land 0xFF);
    Registers.set cpu.v_registers 0xF ((vx land 0x80) lsr 7);
  | _ -> ()

let execute_0x9xy0_opcode cpu instruction =
  let x = (instruction land 0x0F00) lsr 8 in
  let y = (instruction land 0x00F0) lsr 4 in
  let vy = Registers.get cpu.v_registers y in
  let vx = Registers.get cpu.v_registers x in
  if vx != vy then
    cpu.pc <- cpu.pc + 4
  else
    cpu.pc <- cpu.pc + 2

let execute_0xAnnn_opcode cpu instruction =
  cpu.i_register <- (instruction land 0x0FFF);
  cpu.pc <- cpu.pc + 2;
  ()

let execute_0xBnnn_opcode cpu instruction =
  let v0 = Registers.get cpu.v_registers 0 in
  cpu.pc <- ((instruction land 0x0FFF) + v0);
  ()

let execute_0x_Cxkk_opcode cpu instruction =
  let x = (instruction land 0x0F00) lsr 8 in
  let kk = (instruction land 0x00FF) in
  let rnd = (Random.int 256) land kk in
  Registers.set cpu.v_registers x rnd;
  cpu.pc <- cpu.pc + 2;
  ()

let execute_0xDxyn_opcode cpu memory display instruction =
  let vx = (instruction land 0x0F00) lsr 8 in
  let vy = (instruction land 0x00F0) lsr 4 in
  let height = instruction land 0x000F in
  let x_pos = Registers.get cpu.v_registers vx in
  let y_pos = Registers.get cpu.v_registers vy in

  (* Reset collision flag *)
  Registers.set cpu.v_registers 0xF 0;
  let collision = ref false in

  (* Loop over sprite height *)
  for yline = 0 to height - 1 do
    (* Ensure memory access is in bounds *)
    if cpu.i_register + yline < 4096 then begin
      let sprite_byte = Memory.get_byte memory (cpu.i_register + yline) in

      (* Loop over sprite width (always 8 pixels) *)
      for xline = 0 to 7 do
        let sprite_pixel = (sprite_byte land (0x80 lsr xline)) <> 0 in
        let screen_x = x_pos + xline in
        let screen_y = y_pos + yline in

        if sprite_pixel then begin
          if Display.cell_at display screen_x screen_y then collision := true;
          Display.set_cell_at display screen_x screen_y (not (Display.cell_at display screen_x screen_y));
        end;
      done;
    end;
  done;

  (* Set VF register to 1 if there was a collision *)
  if !collision then Registers.set cpu.v_registers 0xF 1;

  (* Move to next instruction *)
  cpu.pc <- cpu.pc + 2;
  ()

let execute_0xE_opcode cpu keypad instruction =
  let x = (instruction land 0x0F00) lsr 8 in
  let key_index = Registers.get cpu.v_registers x in 
  match (instruction land 0xF0FF) with
  | 0xE09E when Keypad.is_key_pressed keypad key_index ->
      cpu.pc <- cpu.pc + 4
  | 0xE0A1 when not (Keypad.is_key_pressed keypad key_index) ->
      cpu.pc <- cpu.pc + 4 
  | _ ->
      cpu.pc <- cpu.pc + 2
  
let execute_0xF_opcode cpu memory keypad instruction =
  let x = (instruction land 0x0F00) lsr 8 in
  let vx = Registers.get cpu.v_registers x in
  match (instruction land 0xF0FF) with
  | 0xF007 ->
    Registers.set cpu.v_registers x cpu.delay_timer;
  | 0xF00A ->
    let rec wait_for_key () =
      match Keypad.get_first_key_pressed keypad with
      | Some key -> Registers.set cpu.v_registers x key  (* Store key in Vx *)
      | None -> wait_for_key ()  (* Keep looping if no key is pressed *)
    in
    wait_for_key ();
  | 0xF015 ->
    cpu.delay_timer <- vx;
  | 0xF018 ->
    cpu.sound_timer <- vx;
  | 0xF01E ->
    cpu.i_register <- cpu.i_register + vx;
  | 0xF029 ->
    cpu.i_register <- 0x050 + vx * 5 (* correct value should be computed either by memory or by font *)
  | 0xF033 ->
    (* Store the hundreds digit at I *)
    Memory.set_byte memory cpu.i_register (vx / 100);
    (* Store the tens digit at I + 1 *)
    Memory.set_byte memory (cpu.i_register + 1) ((vx / 10) mod 10);
    (* Store the ones digit at I + 2 *)
    Memory.set_byte memory (cpu.i_register + 2) (vx mod 10);
  | 0xF055 ->
    (* Store values from V0 to Vx in memory starting at I *)
    for i = 0 to x do
      Memory.set_byte memory (cpu.i_register + i) (Registers.get cpu.v_registers i)
    done;
  | 0xF065 ->
    (* Load values from memory starting at I into registers V0 to Vx *)
    for i = 0 to x do
      Registers.set cpu.v_registers i (Memory.get_byte memory (cpu.i_register + i))
    done;
  | _ -> ()

let execute_opcode cpu memory display keypad instruction = 
  match (instruction land 0xF000) with
  | 0x0000 ->
    execute_0x0_opcode cpu display instruction;
  | 0x1000 ->
    execute_0x1nnn_opcode cpu instruction;
  | 0x2000 ->
    execute_0x2nnn_opcode cpu instruction;
  | 0x3000 ->
    execute_0x3xkk_opcode cpu instruction;
  | 0x4000 ->
    execute_0x4xkk_opcode cpu instruction;
  | 0x5000 ->
    execute_0x5xy0_opcode cpu instruction;
  | 0x6000 ->
    execute_0x6xkk_opcode cpu instruction;
  | 0x7000 ->
    execute_0x7xkk_opcode cpu instruction;
  | 0x8000 ->
    execute_0x8_opcode cpu instruction;
    cpu.pc <- cpu.pc + 2;
  | 0x9000 ->
    execute_0x9xy0_opcode cpu instruction;
  | 0xA000 ->
    execute_0xAnnn_opcode cpu instruction;
  | 0xB000 ->
    execute_0xBnnn_opcode cpu instruction;
  | 0xC000 ->
    execute_0x_Cxkk_opcode cpu instruction;
  | 0xD000 ->
    execute_0xDxyn_opcode cpu memory display instruction;
  | 0xE000 ->
    execute_0xE_opcode cpu keypad instruction;
  | 0xF000 ->
    execute_0xF_opcode cpu memory keypad instruction;
    cpu.pc <- cpu.pc + 2;
  | _ ->
    cpu.pc <- cpu.pc + 2;
    ()


let step cpu (memory: Memory.t) (display: Display.t) (keypad: Keypad.t) = 
  let instruction = Memory.read_at_address memory cpu.pc in
  execute_opcode cpu memory display keypad instruction;