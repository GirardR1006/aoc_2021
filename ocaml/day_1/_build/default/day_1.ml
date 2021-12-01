open Base
open Stdio

let () =
let find_incr l = List.fold ~init:(Int.max_value,0) l ~f:(fun acc x ->
    let p = fst acc and c = snd acc in
    if x > p then (x, c+1)
    else (x, c))
in
let lines = List.map ~f:(Int.of_string) @@
  In_channel.read_lines "input" in 
printf "Number of increases: %d\n" (snd (find_incr lines))
