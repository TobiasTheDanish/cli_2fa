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
    | [] -> acc
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

let show_totps (ctx:context) = 
  let config_dir = get_config_path () in
  let config_file = (config_dir ^ "/config") in
  verbose_print ctx.verbose "Opening config file\n";

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

  let t = Int64.of_float (Unix.time ()) in
  let time_to_next = 30 - ((Int64.to_int t) mod 30) in
  List.iteri (fun i (name,key) -> 
    Printf.printf "#%d: %s\n" (i+1) name;
    let totp = Totp.gen_totp key 30 6 t in
    Printf.printf "  %s\n" totp
  ) keys;

  Printf.printf "Time to new value: %d sec\n" time_to_next
