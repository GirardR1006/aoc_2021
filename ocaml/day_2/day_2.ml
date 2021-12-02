open Base
open Stdio
module Scanf = Caml.Scanf

type horizontal_pos = X of int

type vertical_pos = Y of int

type aim = A of int

type pos =
  | CrudePosition of horizontal_pos * vertical_pos
  | RefinedPosition of horizontal_pos * vertical_pos * aim

type direction = Forward | Up | Down

let dir_of_string = function
  | "forward" -> Forward
  | "up" -> Up
  | "down" -> Down
  | _ -> failwith "Should not happen"

let () =
  let move_submarine l i =
    let res =
      List.fold ~init:i l ~f:(fun i (pos, value) ->
        match i with
        | CrudePosition (X x, Y y) -> (
          match pos with
          | Forward -> CrudePosition (X (x + value), Y y)
          | Up -> CrudePosition (X x, Y (y + value))
          | Down -> CrudePosition (X x, Y (y - value)))
        | RefinedPosition (X x, Y y, A a) -> (
          match pos with
          | Forward ->
            if a = 0
            then RefinedPosition (X (x + value), Y y, A a)
            else RefinedPosition (X (x + value), Y (y + (value * a)), A a)
          | Up -> RefinedPosition (X x, Y y, A (a - value))
          | Down -> RefinedPosition (X x, Y y, A (a + value))))
    in
    match res with
    | CrudePosition (X x, Y y) | RefinedPosition (X x, Y y, _) -> x * y
  in
  let parse str =
    match String.split ~on:' ' str with
    | [ a; b ] -> (dir_of_string a, Int.of_string b)
    | _ -> failwith "Should also not happen"
  in
  let lines = List.map ~f:parse @@ In_channel.read_lines "input" in
  printf "Crude horizontal position multiplied by depth: %d\n"
    (move_submarine lines (CrudePosition (X 0, Y 0)));
  printf "Horizontal position multiplied by depth after RTFM: %d\n"
    (move_submarine lines (RefinedPosition (X 0, Y 0, A 0)))
