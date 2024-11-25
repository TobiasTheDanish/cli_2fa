open Cli_2fa
open Alcotest

let secret = "12345678901234567890"

let hashes = [
  "cc93cf18508d94934c64b65d8ba7667fb7cde4b0";
  "75a48a19d4cbe100644e8ac1397eea747a2d33ab";
  "0bacb7fa082fef30782211938bc1c5e70416ff44";
  "66c28227d03a2d5529262ff016a1e6ef76557ece";
  "a904c900a64b35909874b33e61c5938a8e15ed1c";
  "a37e783d7b7233c083d4f62926c7a25f238d0316";
  "bc9cd28561042c83f219324d3c607256c03272ae";
  "a4fb960c0bc06e1eabb804e5b397cdc4b45596fa";
  "1b3c89f65e6c9e883012052823443f048b4332db";
  "1637409809a679dc698207310c8c7fc07290d9e5";
]

let test_hmac_sha1 () = 
  List.iteri (fun counter expected ->
    let actual = Hotp.hmac_sha1 secret (Int64.of_int counter) in

    check string "Hashes should match" expected (Sha1.to_hex actual)
  ) hashes

let hotps = [
  755224;
  287082;
  359152;
  969429;
  338314;
  254676;
  287922;
  162583;
  399871;
  520489;
]

let test_hotp () =
  List.iteri (fun counter expected ->
    let actual = Hotp.gen_hotp secret (Int64.of_int counter) 6 in

    check int "HOTP's should match" expected actual
  ) hotps

let totps = [
  "94287082";
  "07081804";
  "14050471";
  "89005924";
  "69279037";
  "65353130";
]

let timestamps = [
  59;
  1111111109;
  1111111111;
  1234567890;
  2000000000;
  20000000000;
]

let test_totp () =
  List.iteri (fun i expected ->
    let t = List.nth timestamps i in
    let actual = Totp.gen_totp secret 30 8 (Int64.of_int t) in

    check string "TOTP's should match" expected actual
  ) totps

let () =
  run "hotp module tests"
    [
      ("HMAC-SHA1", [test_case "HMAC-SHA1 hashes" `Quick test_hmac_sha1]);
      ("HOTP-SHA1", [test_case "SHA1 HOTP's" `Quick test_hotp]);
      ("TOTP-SHA1", [test_case "SHA1 TOTP's" `Quick test_totp]);
    ]
