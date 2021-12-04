open Base
open Stdio
open Caml.Format

let range i = List.init ~f:(fun i -> i) i

let rec two_pow = function 0 -> 1 | 1 -> 2 | n -> 2 * two_pow (n - 1)

let () =
  let scan_bit x i =
    let ith_elt x = Bytes.get x i in
    let res = Bytes.of_char_list @@ [ ith_elt x ] in
    res
  in
  let least_found_bit ll =
    let _, _, b =
      List.fold
        ~init:(0, 0, Bytes.of_string "0")
        ~f:(fun (nz, no, min) x ->
          if Bytes.(x = Bytes.of_string "0")
          then
            if nz + 1 > no
            then (nz + 1, no, Bytes.of_string "1")
            else (nz + 1, no, min)
          else if no + 1 > nz
          then (nz, no + 1, Bytes.of_string "0")
          else (nz, no + 1, min))
        ll
    in
    List.nth_exn (Bytes.to_list b) 0
  in
  let most_found_bit ll =
    let _, _, b =
      List.fold
        ~init:(0, 0, Bytes.of_string "0")
        ~f:(fun (nz, no, maj) x ->
          if Bytes.(x = Bytes.of_string "0")
          then
            if nz + 1 > no
            then (nz + 1, no, Bytes.of_string "0")
            else (nz + 1, no, maj)
          else if no + 1 > nz
          then (nz, no + 1, Bytes.of_string "1")
          else (nz, no + 1, maj))
        ll
    in
    List.nth_exn (Bytes.to_list b) 0
  in
  let parse b =
    let bos = Bytes.of_string b in
    List.map ~f:(fun i -> scan_bit bos i) (range (Bytes.length bos))
  in
  let lines = List.map ~f:parse @@ In_channel.read_lines "input" in
  let len_byte = List.length @@ List.nth_exn lines 0 in
  let get_j_col j l = List.map ~f:(fun x -> List.nth_exn x j) l in
  let all_colls l = List.map ~f:(fun j -> get_j_col j l) (range len_byte) in
  let gamma =
    Bytes.of_char_list @@ List.map ~f:most_found_bit (all_colls lines)
  in
  let epsilon =
    Bytes.of_char_list @@ List.map ~f:least_found_bit (all_colls lines)
  in
  printf "gamma: %d\n" (Int.of_string ("0b" ^ Bytes.to_string gamma));
  printf "epsilon: %d\n" (Int.of_string ("0b" ^ Bytes.to_string epsilon))
