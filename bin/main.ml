open Chip8


let () = 
  let cpu = Cpu.create () in
  let rom = Rom.read "roms/IBM_Logo.ch8" in
  let memory = 
    let mem = Memory.create () in
    Memory.load_rom mem rom 
  in
  let rec loop cpu memory =
    let cpu_state = Cpu.step cpu memory in
    loop cpu_state memory
  in
  loop cpu memory
