type command =
  | Register of string option
  | Unknown

let display_usage ?(msg="") () = (
  Printf.printf "%s\n\n" msg;
  Printf.printf "Usage: cli_2fa [command] [options]\n\n";
  Printf.printf "Commands:\n";
  Printf.printf "  register -r [key] : Register a new key for TOTP\n";
  Printf.printf "  help -h           : Prints this message\n"
)

let () = 
  let rec parse_args (argv:string list) acc =
    match argv with
    | [] -> acc
    | arg::rest -> 
        match arg with
        | "register" | "-r" -> (
          match rest with
          | [] -> parse_args rest (Register None)
          | k::rest -> parse_args rest (Register (Some k))
        );
        | "help" | "-h" -> (
          display_usage ();
          exit 1
          )
        | _ -> (
          display_usage ~msg:("Unknown command: " ^ arg) ();
          exit 1
        )
  in

  let argv = Sys.argv in

  match Array.to_list argv with
  | [] -> display_usage ()
  | [_] -> display_usage ()
  | _ :: args ->  
    let cmd = parse_args args Unknown in
    Printf.printf "Found command: %s\n" (
      match cmd with
      | Register _ -> "register"
      | Unknown -> "unknown"
    );
