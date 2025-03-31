type t = {
  mutable display: bool array array
}

let create () = {
  display = Array.make_matrix 32 64 false
}

let cell_at display x y =
  let x' = x land 0x3F in  (* Wrap x coordinate *)
  let y' = y land 0x1F in  (* Wrap y coordinate *)
  display.display.(y').(x')

let set_cell_at display x y value =
  let x' = x land 0x3F in  (* Wrap x coordinate *)
  let y' = y land 0x1F in  (* Wrap y coordinate *)
  display.display.(y').(x') <- value


let clear display =
  for y = 0 to 31 do
    for x = 0 to 63 do
      display.display.(y).(x) <- false
    done
  done
