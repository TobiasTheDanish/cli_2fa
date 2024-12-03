open Cli_2fa.Hotp

let () = 
  let hotp = gen_hotp "12345678901234567890" "THISISATEST" 6 in
  Printf.printf "hotp: %d\n" hotp;
