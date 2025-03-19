
type t = {
  mutable v : int array;
  sp : int;
  mutable stack : int array;
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
    reg.v.(index) <- value;
  ()

let add_value reg index value =
  if index < 0 || index >= 16 then
    failwith "Register index must be between V0 and VF"
  else
    reg.v.(index) <- reg.v.(index) + value;
  ()

let get reg index = reg.v.(index)

let contains reg index value = 
  reg.v.(index) == value

let are_equal reg x_index y_index = reg.v.(x_index) == reg.v.(y_index)