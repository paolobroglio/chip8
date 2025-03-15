type t = {
  data : int array;
}

let read filename = 
  let content = In_channel.with_open_bin filename In_channel.input_all in
  let byte_array = Bytes.of_string content in
  let data = Array.init (Bytes.length byte_array) (fun i -> Char.code (Bytes.get byte_array i)) in
  {
    data = data;
  }