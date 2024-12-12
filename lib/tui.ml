let fps = 30

type event =
  | Key of char

let set_mode termios =
  Unix.tcsetattr Unix.stdin Unix.TCSANOW termios

let set_raw_mode () =
  let termios = Unix.tcgetattr Unix.stdin in
  let raw_termios = { termios with c_icanon = false; c_echo = false} in
  set_mode raw_termios

let poll_events () : event list = 
  let ch = input_char stdin in
  [Key ch]

let loop (initial:'a) (render:('a -> float -> unit)) (update: ('a -> float -> event list -> 'a)) =
  let original_termios = Unix.tcgetattr Unix.stdin in

  try
    set_raw_mode ();

    let state = ref initial in
    let delta = Float.div 1.0 (Float.of_int fps) in
    let last_t = ref (Unix.gettimeofday ()) in

    while true do 
      let cur_t = Unix.gettimeofday () in
      let frame_delta = Float.sub cur_t !last_t in
      if frame_delta >= delta then (
        (* RENDER *)
        render !state frame_delta;
        Stdlib.flush Stdlib.stdout;

        (* POLL EVENTS *)
        let events = poll_events () in

        (* UPDATE *)
        state := update !state frame_delta events ;
        last_t := cur_t
      )
    done
  with exn ->
    set_mode original_termios;
    raise exn


(* RIOT IMPLEMENTATION INCOMING!!*)

open Tty

type custom_time = float

type modifier = No_modifier | Ctrl

let modifier_to_string = function
  | No_modifier -> ""
  | Ctrl -> "<CTRL>"

type key = 
  | Up
  | Down
  | Left
  | Right
  | Space
  | Escape
  | Backspace
  | Enter
  | Key of string

let key_to_string = function
  | Up -> "<up>"
  | Down -> "<down>"
  | Left -> "<left>"
  | Right -> "<right>"
  | Space -> "<space>"
  | Escape -> "<esc>"
  | Backspace -> "<backspace>"
  | Enter -> "<enter>"
  | Key key -> key

type riot_event = 
  | KeyDown of key * modifier
  | Frame of custom_time
  | Unknown of Riot.Message.t

type Riot.Message.t += 
  | Input of riot_event

let io_run updater = 
  let translate = function
    | " " -> Space
    | "\027" -> Escape
    | "\027[A" -> Up
    | "\027[B" -> Down
    | "\027[C" -> Right
    | "\027[D" -> Left
    | "\127" -> Backspace
    | "\n" -> Enter
    | key -> Key key
  in

  let rec loop updater =
    Riot.yield ();
    match Stdin.read_utf8 () with
    | `Read key ->
        let msg =
          match key with
          | "\027" -> (
              match Stdin.read_utf8 () with
              | `Read "[" -> (
                  match Stdin.read_utf8 () with
                  | `Read key -> KeyDown (translate ("\027[" ^ key), No_modifier)
                  | _ -> KeyDown (translate key, No_modifier))
              | _ -> KeyDown (translate key, No_modifier))
          | "\n" -> KeyDown (translate key, No_modifier)
          | key when key >= "\x01" && key <= "\x1a" ->
              let key =
                key.[0] |> Char.code |> ( + ) 96 |> Char.chr |> String.make 1
              in
              KeyDown (translate key, Ctrl)
          | key -> KeyDown (translate key, No_modifier)
        in
        Riot.send updater (Input msg);
        loop updater
    | _ -> loop updater
  in

  Riot.link updater;
  let termios = Stdin.setup () in
  let _ = Riot.spawn_link (fun () -> loop updater) in
  let _ = Riot.receive_any () in
  Stdin.shutdown termios

type 'model app = {
  _timer: Riot.Timer.timer;
  mutable state: 'model;
  render:('model -> unit);
  update:('model -> riot_event -> 'model);
  start_time: custom_time;
}

type Riot.Message.t += 
  | Tick

let updater_run (app: 'model app) =
  let rec loop (app: 'model app) =
    let e =
      match Riot.receive_any () with
      | Input e -> e
      | Tick -> Frame ((Unix.gettimeofday ()) -. app.start_time)
      | msg -> Unknown msg
    in

    match e with
    | Unknown _ -> Printf.eprintf "Unknown message\n"; Riot.exit (Riot.self ()) Riot.Process.Normal
    | e -> 
        app.state <- app.update app.state e;
        app.render app.state;
        Stdlib.flush Stdlib.stdout;
        loop app
  in

  app.state <- app.update app.state (Frame 0.0);
  app.render app.state;
  Stdlib.flush Stdlib.stdout;

  loop app

let fps_to_interval (fps:int) =
  1. /. (Int.to_float fps) *. 1_000.
    |> Int64.of_float

let loop_riot (initial:'model) (render:('model -> unit)) (update: ('model -> riot_event -> 'model)) =
  Sys.set_signal Sys.sigint (Signal_handle (fun _ -> Ansi.show_cursor(); exit 0));

  Riot.run @@ fun () -> 
    let updater = Riot.spawn (fun () -> 
      let timer = 
        Riot.Timer.send_interval ~every:(fps_to_interval fps) (Riot.self ()) Tick 
        |> Result.get_ok
      in
      let app = {
        _timer = timer;
        state = initial;
        render = render;
        update = update;
        start_time = (Unix.gettimeofday ())
      } in

      updater_run app
    ) in

    let io = Riot.spawn (fun () -> io_run updater) in

    Riot.wait_pids [updater;];

    Riot.exit io Riot.Process.Normal
    |> Riot.shutdown
