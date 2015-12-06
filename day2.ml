(* compile with:
 * ocamlfind ocamlc -linkpkg -thread -package core day2.ml -o day2
 * run with:
 * ./day2 < day2-input.txt
 *)

open Core.Std;;

let surface l w h =
  2*l*w + 2*w*h + 2*h*l;;
let extra l w h =
  min (min (l*w) (w*h)) (h*l);;

let total l w h = surface l w h + extra l w h;;
let total_list (l::w::[h]) = total l w h;;

(* Example call: total_list (List.map ~f:int_of_string (String.split ~on:'x' "3x11x24"));; *)

let build_areas () =
  In_channel.fold_lines stdin ~init:[] ~f:(fun counts line ->
    total_list (List.map ~f:int_of_string (String.split ~on:'x' line)) :: counts )
;;

let () =
  build_areas ()
  |> List.fold ~init:0 ~f:(+)
  |> printf "%3d\n";;
 (* |> List.iter ~f:(fun count -> printf "%3d\n" count);; *)


