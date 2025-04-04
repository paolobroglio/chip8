type instruction = int

type t = {
  ram: instruction array;
}

let create () = {
  ram = Array.make 4096 0;
}

let set_byte memory address byte =
  memory.ram.(address) <- byte

let get_byte memory address =
  if address >= Array.length memory.ram then
    failwith "Memory overflow when requesting value"
  else
    memory.ram.(address)

let load_font memory =
  let font = Font.make in
  let start_addr = 0x050 in
  Array.iteri (fun i byte ->
    if start_addr + i < Array.length memory.ram then
      memory.ram.(start_addr + i) <- byte
    else
      failwith "Memory overflow while loading ROM"
  ) font.fontset; 
  memory

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
