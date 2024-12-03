open Cli_2fa.Totp

let () = 
  let t = Int64.of_float (Unix.time ()) in

  let totp = gen_totp "JBSWY3DPEHPK3PXP" 30 6 t in
  Printf.printf "totp: %s\n" totp;
