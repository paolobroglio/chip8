
type t = {
  v : int array;
  sp : int;
  stack : int array;
}

let create () = {
  v = Array.make 16 0;
  sp = 0;
  stack = Array.make 16 0;
}

let set reg index value = 
  if index < 0 || index >= 16 then
    failwith "Register index must be between V0 and VF"
  else
    let reg_state = { v = Array.copy reg.v; sp = reg.sp; stack = reg.stack} in
    reg_state.v.(index) <- value;
    reg_state

let get reg index = reg.v.(index)

let contains reg index value = reg.v.(index) == value

let are_equal reg x_index y_index = reg.v.(x_index) == reg.v.(y_index)
