type t = {
  mutable kp: bool array;
}

let create () = {
  kp = Array.make 16 false;
}

let reset keypad = 
  keypad.kp <- Array.make 16 false

let set_key keypad key value =
  if key >= 0 && key < 16 then
    keypad.kp.(key) <- value
  else
    failwith "key out of keypad range"

let set_key_pressed keypad key =
  set_key keypad key true

let set_key_released keypad key =
  set_key keypad key false

let is_key_pressed keypad key =
  if key >= 0 && key < 16 then
    keypad.kp.(key)
  else
    failwith "key out of keypad range"

let get_first_key_pressed keypad =
  let rec search i =
    if i >= Array.length keypad.kp then None
    else if keypad.kp.(i) == true then Some i
    else search (i + 1)
  in
  search 0
  

