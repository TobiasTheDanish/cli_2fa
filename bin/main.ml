open Shared.Types
open Cli_2fa

let display_usage ?(msg="") ?(name="cli_2fa") () = (
  if String.length msg > 0 then
    Printf.printf "%s\n\n" msg;
  
  Printf.printf "Usage: %s [command] [options]\n\n" name;
  Printf.printf "Commands:\n";
  Printf.printf "  register -r [key] : Register a new key for TOTP\n";
  Printf.printf "  help -h           : Prints this message\n\n";
  Printf.printf "Options:\n";
  Printf.printf "  verbose -v        : Output all info\n"
)

let _debug_context (ctx:context) = 
  Printf.printf "Context:\n";
  Printf.printf "  cmd: %s\n" (
    match ctx.cmd with
        | Unknown -> "unknown"
        | Register k -> (
          match k with 
              | Some k -> "register with '" ^ k ^ "'"
              | None -> "register with no key"
        )
  );
  Printf.printf "  verbose: %s\n" (
    match ctx.verbose with
    | true -> "true"
    | false -> "false"
  )

let run (ctx:context) =
  match ctx.cmd with
  | Unknown -> failwith "Got a unknown command, this should never happen!"
  | Register k -> Command.register_key k ctx

let () = 
  let rec parse_args (argv:string list) (acc:context) =
    match argv with
    | [] -> acc
    | arg::rest -> 
        match arg with
        | "register" | "-r" -> (
          if acc.cmd <> Unknown then (
            display_usage ~msg:"Too many commands found. You can only perform 1 at a time" ();
            exit 1
          ) else (
            match rest with
            | [] -> parse_args rest ({
              cmd = Register None;
              verbose = acc.verbose;
            })
            | k::rest -> parse_args rest ({
              cmd = Register (Some k);
              verbose = acc.verbose;
            })
          )
        );
        | "verbose" | "-v" -> (
          parse_args rest {
            cmd = acc.cmd;
            verbose = true;
          }
        )
        | "help" | "-h" -> (
          display_usage ();
          exit 0
          )
        | _ -> (
          display_usage ~msg:("Unknown command: " ^ arg) ();
          exit 1
        )
  in

  let argv = Sys.argv in

  match Array.to_list argv with
  | []  -> display_usage ()
  | [name] -> display_usage ~name:name ()
  | _ :: args ->  
      let ctx = parse_args args {cmd = Unknown; verbose = false;} in
      _debug_context ctx;
      run ctx
