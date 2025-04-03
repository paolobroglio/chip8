open Raylib
open Chip8

let usage_msg = "chip8 [-verbose] <rom_file>"
let verbose = ref false
let input_files = ref []
let anon_fun filename = input_files := filename :: !input_files

let speclist = 
  [("-verbose", Arg.Set verbose, "Debug logs")]

let get_rom list_ref index =
  try
    List.nth !list_ref index
  with
  | Failure _ -> failwith "ROM file not provided!"

let main () =
  Arg.parse speclist anon_fun usage_msg;
  let rom_file = get_rom input_files 0 in

  Raylib.init_window 640 320 "CHIP-8 Emulator";
  Raylib.set_target_fps 60;

  (* CHIP-8 Setup *)
  let cpu = Cpu.create () in
  let rom = Rom.read rom_file in
  let memory = 
    let mem = Memory.create () in
    Memory.load_rom mem rom |> Memory.load_font
  in
  let display = Display.create () in

  let rec loop cpu memory display =
    if Raylib.window_should_close () then
      Raylib.close_window ()
    else
      begin
        
        let elapsed_time = Raylib.get_frame_time () in
        let instructions_per_frame = 10 in
        for _ = 1 to instructions_per_frame do
          Cpu.step cpu memory display;
        done;

        (* Update timers *)
        if elapsed_time >= 1.0 /. 60.0 then
          begin
            cpu.delay_timer <- cpu.delay_timer - 1;
            cpu.sound_timer <- cpu.sound_timer - 1;
            if cpu.sound_timer > 0 then
              (* Play beep sound *)
              ()
          end;

        (* Draw CHIP-8 display *)
        Raylib.begin_drawing ();
        Raylib.clear_background Color.black;
        for y = 0 to 31 do
          for x = 0 to 63 do
            if Display.cell_at display x y then
              Raylib.draw_rectangle (x * 10) (y * 10) 10 10 Color.raywhite
            else
              Raylib.draw_rectangle (x * 10) (y * 10) 10 10 Color.black
          done
        done;
        Raylib.end_drawing ();
        
        loop cpu memory display
      end
  in
  loop cpu memory display

let () = main ()
  