open Base
open Stdio
module Scanf = Caml.Scanf

type horizontal_pos = X of int

type vertical_pos = Y of int

type pos = horizontal_pos * vertical_pos

type direction = Forward | Up | Down

let dir_of_string = function
  | "forward" -> Forward
  | "up" -> Up
  | "down" -> Down
  | _ -> failwith "Should not happen"

let () =
  let move_submarine l =
    List.fold ~init:(X 0, Y 0) l ~f:(fun (X x, Y y) (pos, value) ->
      match pos with
      | Forward -> (X (x + value), Y y)
      | Up -> (X x, Y (y + value))
      | Down -> (X x, Y (y - value)))
  in
  let parse str =
    match String.split ~on:' ' str with
    | [ a; b ] -> (dir_of_string a, Int.of_string b)
    | _ -> failwith "Should also not happen"
  in
  let lines = List.map ~f:parse @@ In_channel.read_lines "input" in
  let X x, Y y = move_submarine lines in
  printf "Horizontal position multiplied by depth: %d\n" (x * y)
