open Chip8
open OUnit2

let test_read_at_address_returns_0x00E0 _ =
  let memory = 
    let mem = Memory.create () in
    Memory.set_byte mem 0x200 0x00;
    Memory.set_byte mem 0x201 0xE0;
    mem
  in 
  assert_equal ~msg:"Expected 0x00E0 at 0x200 address"
    0x00E0 (Memory.read_at_address memory 0x200)

let test_registers_set _ =
  let registers = 
    let reg = Registers.create () in
    Registers.set reg 0 0x111
  in 
  assert_equal ~msg:"Expected 0x111 at V0"
    0x111 (Registers.get registers 0)

let test_registers_contains_a_value _ =
  let registers = 
    let reg = Registers.create () in
    Registers.set reg 0 0x111
  in 
  assert_equal ~msg:"Expected 0x111 at V0"
    true (Registers.contains registers 0 0x111)

let test_registers_are_equal _ =
  let registers = 
    let reg = Registers.create () in
    let regv0set = Registers.set reg 0 0x111 in
    Registers.set regv0set 1 0x111;
  in 
  assert_equal ~msg:"Expected V0 = V1"
    true (Registers.are_equal registers 0 1)

let suite =
  "Chip8 tests" >:::
  [
    "read_at_address_returns_0x00E0" >:: test_read_at_address_returns_0x00E0;
    "registers_set_V0_to_0x111" >:: test_registers_set;
    "registers_V0_contains_0x111" >:: test_registers_contains_a_value;
    "registers_V0_is_equal_to_V1" >:: test_registers_are_equal;
  ]

  
let () = run_test_tt_main suite