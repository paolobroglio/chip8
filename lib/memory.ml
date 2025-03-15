type instruction = int

type t = {
  ram: instruction array;
}

let create () = {
  ram = Array.make 4096 0;
}

let load_rom memory (rom: Rom.t) = 
  let start_addr = 0x200 in
  Array.iteri (fun i byte ->
    if start_addr + i < Array.length memory.ram then
      memory.ram.(start_addr + i) <- byte
    else
      failwith "Memory overflow while loading ROM"
  ) rom.data; 
  memory

let read_at_address memory address = 
  let first_byte = memory.ram.(address) lsl 8 in
  let second_byte = memory.ram.(address + 1) in
  first_byte lor second_byte
