open Shared.Types
open Cli_2fa

let _display_usage ?(msg="") ?(name="cli_2fa") () = (
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
        | Show -> "show"
        | Ansi -> "ansi"
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
  | Show -> Command.show_totps ctx
  | Ansi -> Command.ansi ctx
  | Register k -> Command.register_key k ctx

let _is_flag = function
| "show"     | "-s" 
| "register" | "-r" 
| "help"     | "-h"
| "verbose"  | "-v" -> true
| _ -> false

let verbose = ref false
let command = ref Unknown

let speclist = [
  ("-v", Arg.Set verbose, "Output debug information");
  ("-a", Arg.Unit (fun () -> 
    if !command <> Unknown 
    then failwith "Too many commands" 
    else command := Ansi
  ), "Start ansi encoded terminal");
  ("-s", Arg.Unit (fun () -> 
    if !command <> Unknown 
    then failwith "Too many commands" 
    else command := Show
  ), "Show values for registered keys");
  ("--show", Arg.Unit (fun () -> 
    if !command <> Unknown 
    then failwith "Too many commands" 
    else command := Show
  ), "Show values for registered keys");
  ("-r", Arg.String (fun key -> 
    if !command <> Unknown 
    then failwith "Too many commands" 
    else command := Register (Some key)
  ), "Register new key");
  ("--register", Arg.String (fun key -> 
    if !command <> Unknown 
    then failwith "Too many commands" 
    else command := Register (Some key)
  ), "Register new key");
]

let anon_fun arg = 
  Arg.usage speclist ("Unknown arg '" ^ arg ^ "'.\n")

let () = 
  Arg.parse speclist anon_fun "";
  let ctx: context = ({
    cmd = !command;
    verbose = !verbose;
  }) in

  _debug_context ctx;
  run ctx

(* let () =  *)
(*   let rec parse_args (argv:string list) (acc:context) = *)
(*     match argv with *)
(*     | [] -> acc *)
(*     | arg::rest ->  *)
(*         match arg with *)
(*         | "show" | "-s" -> ( *)
(*           if acc.cmd <> Unknown then ( *)
(*             display_usage ~msg:"Too many commands found. You can only perform 1 at a time" (); *)
(*             exit 1 *)
(*           ) else  *)
(*             parse_args rest ({ *)
(*               cmd = Show; *)
(*               verbose = acc.verbose; *)
(*             }) *)
(*         ); *)
(*         | "register" | "-r" -> ( *)
(*           if acc.cmd <> Unknown then ( *)
(*             display_usage ~msg:"Too many commands found. You can only perform 1 at a time" (); *)
(*             exit 1 *)
(*           ) else ( *)
(*             match rest with *)
(*             | [] -> parse_args rest ({ *)
(*               cmd = Register None; *)
(*               verbose = acc.verbose; *)
(*             }) *)
(*             | k::args -> if is_flag k then *)
(*               parse_args rest ({ *)
(*                 cmd = Register None; *)
(*                 verbose = acc.verbose; *)
(*               }) *)
(*             else parse_args args ({ *)
(*               cmd = Register (Some k); *)
(*               verbose = acc.verbose; *)
(*             }) *)
(*           ) *)
(*         ); *)
(*         | "verbose" | "-v" -> ( *)
(*           parse_args rest { *)
(*             cmd = acc.cmd; *)
(*             verbose = true; *)
(*           } *)
(*         ) *)
(*         | "help" | "-h" -> ( *)
(*           display_usage (); *)
(*           exit 0 *)
(*           ) *)
(*         | _ -> ( *)
(*           display_usage ~msg:("Unknown command: " ^ arg) (); *)
(*           exit 1 *)
(*         ) *)
(*   in *)
(**)
(*   let argv = Sys.argv in *)
(**)
(*   match Array.to_list argv with *)
(*   | []  -> display_usage () *)
(*   | [name] -> display_usage ~name:name () *)
(*   | _ :: args ->   *)
(*       let ctx = parse_args args {cmd = Unknown; verbose = false;} in *)
(*       _debug_context ctx; *)
(*       run ctx *)
