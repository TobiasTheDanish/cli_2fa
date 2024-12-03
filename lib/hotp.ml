let sha1_block_size = 64

let xor_string s1 s2 =
  let pad_to_size s l =
    match String.length s with
    | x when x >= l -> s
    | _ -> (
      String.cat 
        s
        (String.make (l - String.length s) (char_of_int 0)) 
      )
  in

  let xor_char c1 c2 =
    char_of_int ((int_of_char c1) lxor (int_of_char c2))
  in
  let max_len = match (String.length s1), (String.length s2) with
  | (l1, l2) when l1 >= l2 -> l1
  | (_, l2) -> l2
  in
  let ps1 = pad_to_size s1 max_len in
  let ps2 = pad_to_size s2 max_len in

  String.mapi (fun i c -> xor_char c (String.get ps2 i)) ps1

let hmac_sha1 (key:string) (msg:string) : string =
  let opad = String.make sha1_block_size (char_of_int 0x5c) in
  let ipad = String.make sha1_block_size (char_of_int 0x36) in
  let key_len = String.length key  in
  let k' = match key_len with
  | _ when key_len > sha1_block_size -> Sha1.string key |> Sha1.to_bin
  | _ -> key 
  in
  let k_ipad = xor_string k' ipad in
  let k_ipad_msg = String.cat k_ipad msg in
  let h1 = Sha1.string k_ipad_msg |> Sha1.to_bin in
  let k_opad = xor_string k' opad in
  let res = Sha1.string (String.cat k_opad h1) in
  Printf.printf "Sha1: %s\n" (Sha1.to_hex res);
  Sha1.to_bin res

let mac_offset (s:string) : int = 
  let n = String.length s in
  let c = String.get s (n-1) in
  (int_of_char c) land 0xf

let mac_bin_code (s:string) (offset:int) : int =
  let x1 = int_of_char (String.get s offset) in
  let x2 = int_of_char (String.get s (offset+1)) in
  let x3 = int_of_char (String.get s (offset+2)) in
  let x4 = int_of_char (String.get s (offset+3)) in

  ((x1 land 0x7f) lsl 24)
  lor ((x2 land 0xff) lsl 16)
  lor ((x3 land 0xff) lsl 8)
  lor (x4 land 0xff)

let rec pow n e acc =
  match e with
  | _ when e <= 0 -> acc
  | _ -> pow n (e-1) (n * acc)

let gen_hotp (key:string) (counter:string) (digits:int): int =
  let sha = hmac_sha1 key counter in
  let offset = mac_offset sha in
  let bin_code = mac_bin_code sha offset in

  bin_code mod (pow 10 digits 1)
