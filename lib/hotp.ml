let sha1_block_size = 64

let hmac_sha1 (key:string) (m:int64) : Sha1.t =
  let counter_bytes =
    Bytes.init 8 (fun i ->
        Char.chr ((Int64.to_int (Int64.shift_right m (8 * (7 - i)))) land 0xFF))
  in

  let msg = Bytes.to_string counter_bytes in

  let key_len = String.length key  in
  let k' = match key_len with
  | _ when key_len > sha1_block_size -> 
      Sha1.string key 
      |> Sha1.to_bin
  | _ when key_len < sha1_block_size ->
      key ^ (String.make (sha1_block_size-key_len) '\000')
  | _ -> key 
  in
  
  let ipad = Bytes.init sha1_block_size (fun i -> Char.chr ((Char.code k'.[i]) lxor 0x36)) in
  let opad = Bytes.init sha1_block_size (fun i -> Char.chr ((Char.code k'.[i]) lxor 0x5c)) in

  let inner_hash = Sha1.string (Bytes.to_string ipad ^ msg) in
  let res = Sha1.string (Bytes.to_string opad ^ Sha1.to_bin inner_hash) in

  Printf.printf "sha: %s\n" (Sha1.to_hex res);
  res

let mac_offset (s:string) : int = 
  let n = String.length s in
  let c = int_of_char (String.get s (n-1)) in
  c land 0xf

let mac_bin_code (s:string) (offset:int) : int =
  let x1 = int_of_char (String.get s offset) in
  let x2 = int_of_char (String.get s (offset+1)) in
  let x3 = int_of_char (String.get s (offset+2)) in
  let x4 = int_of_char (String.get s (offset+3)) in

  Printf.printf "bytes from offset: %2x%2x%2x%2x\n" x1 x2 x3 x4;

  ((x1 land 0x7f) lsl 24)
  lor ((x2 land 0xff) lsl 16)
  lor ((x3 land 0xff) lsl 8)
  lor (x4 land 0xff)

let rec pow n e acc =
  match e with
  | _ when e <= 0 -> acc
  | _ -> pow n (e-1) (n * acc)

let gen_hotp (key:string) (counter:int64) (digits:int): int =
  Printf.printf "counter:%d\n" (Int64.to_int counter);
  let sha = hmac_sha1 key counter
    |> Sha1.to_bin
  in
  let offset = mac_offset sha in
  Printf.printf "offset: %d\n" offset;
  let bin_code = mac_bin_code sha offset in
  let max = (pow 10 digits 1) in

  bin_code mod max
