open Shared.Types

type key = (string * string)

let verbose_print v msg =
  if v then (
    Printf.printf "%s" msg;
    Stdlib.flush Stdlib.stdout
  )

let get_env (name:string) = 
  Sys.getenv_opt name

let open_dir (path:string) (v:bool) =
  try 
    if not (Sys.file_exists path && Sys.is_directory path) then (
      verbose_print v ("No directory at '" ^ path ^ "'. Creating dir\n");
      Unix.mkdir path 0o755
    )
  with
  | Sys_error err -> Printf.eprintf "Error when creating directory at '%s': %s\n" path err
  | Unix.Unix_error _ -> Printf.eprintf "Error when creating directory at '%s'\n" path

let open_read_file (path:string) =
  try 
    let ic = open_in_gen [Open_creat;Open_rdonly;Open_text] 0o444 path in
    Some ic
  with
  | _ -> 
      Printf.eprintf "Error opening file at '%s'\n" path;
      None

let open_append_file (path:string) = 
  try
    let oc = open_out_gen [Open_creat;Open_append;Open_text] 0o644 path  in
    Some oc
  with
  | _ -> 
      Printf.eprintf "Error opening file at '%s'\n" path;
      None

let read_key_from_stdin () =
  Printf.printf "Please provide the key you wish to register:\n";
  read_line ()

let get_config_path () =
  let home = get_env "HOME" in
  let home_dir = match home with
  | Some s -> s
  | None -> failwith "'HOME' env var not set" 
  in

  home_dir ^ "/.cli_2fa"

let register_key (key_opt:string option) (ctx:context) =
  let key = match key_opt with
  | Some k -> k
  | None -> read_key_from_stdin ()
  in

  Printf.printf "What name would you like to reference this key by?\n";
  let name = read_line () in

  let config_dir = get_config_path () in
  open_dir config_dir ctx.verbose;

  let config_file = (config_dir ^ "/config") in
  verbose_print ctx.verbose "Opening config file\n";
  let oc = open_append_file config_file in
  match oc with
  | None -> ()
  | Some oc -> 
      try
      Printf.fprintf oc "%s : %s\n" name key;
      Stdlib.flush oc;

      Printf.printf "Key registered successfully\n";
      close_out oc
      with
      | Sys_error e -> (
        Printf.eprintf "Error writing to file '%s': '%s'\n" config_file e;
        close_out_noerr oc
      )

let riot_register (ctx:context) =


let read_lines (ic:in_channel) = 
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> List.rev acc in
  loop []

let lines_to_keys (lines:string list) : key list =
  let rec loop (lines:string list) (acc:key list) =
    match lines with
    | [] -> List.rev acc
    | line::lines -> (
      if String.length line = 0 then 
        loop lines acc
      else 
        let parts = String.split_on_char ':' line in
        match parts with
        | [name;key] -> (
          loop lines (((String.trim name), (String.trim key)) :: acc)
        )
        | _ -> failwith "Unknown format in config file\n"
    )
  in
  loop lines []

type show_state = {
  time_to_next : int;
  values : (string * string * string) list ;
  cursor_row: int;
}

let show_totps (_ctx:context) = 
  let config_dir = get_config_path () in
  let config_file = (config_dir ^ "/config") in

  let ic = open_read_file config_file in
  let keys = match ic with
  | None -> (
      Printf.eprintf "No config file found. Register some keys first\n";
      []
    )
  | Some ic ->
      let lines = read_lines ic in
      lines_to_keys lines
  in

  let render (state:show_state) =
    Ansi.clear_screen () |> Ansi.move_cursor_home;
    Ansi.output_line ("Next value in " ^ (Int.to_string state.time_to_next) ^ " seconds\n" );

    List.iteri (fun i (name,_, totp) -> 
      Ansi.output "Key #";
      Ansi.output (Int.to_string (i+1));
      Ansi.output ": ";
      Ansi.set_color Bold Default Default;
      Ansi.output_line name;
      Ansi.set_graphic_mode Reset;

      Ansi.move_cursor_right 4;
      Ansi.set_color Bold Default Default;
      if state.time_to_next <= 5 then (
        if (state.time_to_next mod 2) = 1 then
          Ansi.set_color Bold Red Default
      );
      if state.cursor_row = i then (
        Ansi.set_color Bold Cyan Default;
        Ansi.move_cursor_left 2;
        Ansi.output "X "
      );
      Ansi.output_line totp;
      Ansi.set_graphic_mode Reset;
    ) state.values
  in

  let update (state:show_state) (event:Tui.riot_event) =
    match event with
    | Frame new_t -> (
      let time_to_next = 30 - ((Int.of_float new_t) mod 30) in
      {
        time_to_next = time_to_next;
        values = if time_to_next = 30 then
          List.map (fun (n, k, _) -> 
            let t = Int64.of_float new_t in
            let totp = Totp.gen_totp k 30 6 t in
            (n, k, totp)
          ) state.values
        else state.values;
        cursor_row = state.cursor_row;
      }
    )
    | KeyDown (k, _) -> (
      let new_cursor = match k with
        | Up -> state.cursor_row - 1
        | Key k when k = "k" -> state.cursor_row - 1
        | Down -> state.cursor_row + 1
        | Key k when k = "j" -> state.cursor_row + 1
        | _ -> state.cursor_row
      in
      {
        time_to_next = state.time_to_next;
        values = state.values;
        cursor_row = 
          if new_cursor < 0 then (List.length state.values)-1 
          else if new_cursor >= (List.length state.values) then 0
          else new_cursor
        ;
      }
    ) 
    | Unknown _ -> state
  in

  let t = Int64.of_float (Unix.time ()) in
  let time_to_next = 30 - ((Int64.to_int t) mod 30) in

  let values = List.map 
    (fun (n, k) -> (n, k, (Totp.gen_totp k 30 6 t)))
    keys in

  Tui.loop_riot {
    time_to_next = time_to_next;
    values = values;
    cursor_row = 0;
  } render update

let ansi (_ctx:context) =
  Tui.loop_riot (0.0, "none", "none") (fun (t, k, e) -> 
    Ansi.clear_screen () |> Ansi.move_cursor_home;
    Ansi.output ("running time: " ^ (Float.to_string t));
    Ansi.move_cursor_down_start 1;
    Ansi.output ("Last event: " ^ e);
    Ansi.move_cursor_down_start 1;
    Ansi.output ("Last key: " ^ k)
  ) (fun (prevT, prevK, _) event -> match event with
      | KeyDown (k, m) -> (prevT, ((Tui.modifier_to_string m) ^(Tui.key_to_string k)), "KeyDown")
      | Frame t -> (t, prevK, "Frame")
      | Unknown _ -> Printf.eprintf "Unknown message!"; (prevT, prevK, "");
  )
