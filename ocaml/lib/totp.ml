open Hotp

let decode_base32_key (key:string) =
  match Base32.decode key with
  | Ok decoded -> decoded
  | Error _ -> failwith "Invalid base32 key"

let gen_totp (key:string) (time_step:int) (digits:int) (t:int64) : string = 
  let counter = (Int64.div t (Int64.of_int time_step)) in

  let otp = gen_hotp (decode_base32_key key) counter digits 
  |> string_of_int in
  
  if String.length otp < digits then
    String.make (digits-(String.length otp)) '0' ^ otp
  else
    otp

