open Shared.Types

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

let open_file (path:string) = 
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

let register_key (key_opt:string option) (ctx:context) =
  let key = match key_opt with
  | Some k -> k
  | None -> read_key_from_stdin ()
  in

  Printf.printf "What name would you like to reference this key by?\n";
  let name = read_line () in

  let home = get_env "HOME" in
  let home_dir = match home with
  | Some s -> s
  | None -> failwith "'HOME' env var not set" 
  in

  let config_dir = home_dir ^ "/.cli_2fa" in
  open_dir config_dir ctx.verbose;

  let config_file = (config_dir ^ "/config") in
  verbose_print ctx.verbose "Opening config file\n";
  let oc = open_file config_file in
  match oc with
  | None -> ()
  | Some oc -> 
      try
      Printf.fprintf oc "name : %s\nkey : %s\n" name key;
      Stdlib.flush oc;

      Printf.printf "Key registered successfully\n";
      close_out oc
      with
      | Sys_error e -> (
        Printf.eprintf "Error writing to file '%s': '%s'\n" config_file e;
        close_out_noerr oc
      )
