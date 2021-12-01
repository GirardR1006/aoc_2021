open Base
open Stdio

let () =
let find_incr l i = List.fold ~init:i l ~f:(fun acc x ->
      match acc with
      | [p; o] -> if x > p then [x; o+1] else [x; o]
      | [last; mid; first; objective; count] ->
        if count = 3 then(
          if mid + first + x > mid + first + last then
            [mid; first; x; objective +1; 3]
          else
            [mid; first; x; objective ; 3]
        )
        else
          [mid; first; x; objective; count + 1]
      | _ -> failwith "should not happen"
    )
in
let lines = List.map ~f:(Int.of_string) @@
  In_channel.read_lines "input" in
printf "Number of increases: %d\n"
  (List.nth_exn  (find_incr lines [Int.max_value;0]) 1);
printf "Number of increases within measuring windows: %d\n"
  (List.nth_exn  (find_incr lines [0;0;0;0;0]) 3);
