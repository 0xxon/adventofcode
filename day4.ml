(* corebuild -pkg Cryptokit day4.native *)

(* #require "core.top";;
#require "cryptokit";; *)

open Core.Std;;
open Cryptokit;;

let secret_key = "yzbqklnj";;

let result_number_1 =
  let current_string x = secret_key ^ string_of_int x in
  let create_hash x =
    hash_string (Hash.md5()) (current_string x)
    |> transform_string (Hexa.encode() ) in
  let extract_sub x =
    String.sub (create_hash x) 0 5 in
  let rec find_correct x =
    match extract_sub x with
    | "00000" -> x
    | _ -> find_correct (x+1)
   in
   find_correct 0;;

let result_number_2 =
  let current_string x = secret_key ^ string_of_int x in
  let create_hash x =
    hash_string (Hash.md5()) (current_string x)
    |> transform_string (Hexa.encode() ) in
  let extract_sub x =
    String.sub (create_hash x) 0 6 in
  let rec find_correct x =
    match extract_sub x with
    | "000000" -> x
    | _ -> find_correct (x+1)
   in
   find_correct 0;;

let () =
  Out_channel.output_string stdout ("Result 1: " ^ string_of_int result_number_1 ^ "\n");
  Out_channel.output_string stdout ("Result 2: " ^ string_of_int result_number_2 ^ "\n");
;;
