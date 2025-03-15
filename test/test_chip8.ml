open Chip8
open OUnit2


let test_read_at_address_empty _ =
  let memory = Memory.create () in
  assert_equal ~msg:"Expected 0 at empty address"
    0 (Memory.read_at_address memory 0)

let suite =
  "Memory tests" >:::
  [
    "read_at_address_empty" >:: test_read_at_address_empty;
  ]

  
let () = run_test_tt_main suite