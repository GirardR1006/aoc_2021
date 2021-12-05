open Base
open Stdio

let range i = List.init ~f:(fun i -> i) i

let parse_strings s =
  List.map
    ~f:(fun x -> (Int.of_string x, false))
    ((List.filter ~f:(fun s -> String.(not (s = "")))) (String.split ~on:' ' s))

module Board = struct
  type 'a board = { x_size : int; y_size : int; data : 'a list list }
  [@@deriving show, map, fold]

  type t = (int * bool) board [@@deriving show]

  let get b x y = List.nth_exn (List.nth_exn b.data y) x

  let get_col b y = List.map ~f:(fun i -> get b i y) (range b.y_size)

  let get_row b x = List.map ~f:(fun i -> get b x i) (range b.x_size)

  let get_all_cols b = List.map ~f:(fun i -> get_col b i) (range b.x_size)

  let get_all_rows b = List.map ~f:(fun i -> get_row b i) (range b.y_size)

  let mark_by_value b x =
    map_board (fun (v, b) -> if v = x then (v, true) else (v, b)) b

  let is_winner b =
    let rows = get_all_rows b and cols = get_all_cols b in
    let scan = List.fold ~init:true ~f:(fun acc (_, b) -> acc && b) in
    let res =
      List.fold ~f:( || ) ~init:false (List.map ~f:scan rows)
      || List.fold ~f:( || ) ~init:false (List.map ~f:scan cols)
    in
    if res then Some b else None
end

let parse file =
  let head_s, grids_s =
    match In_channel.read_lines file with
    | h :: t -> (h, t)
    | _ -> failwith "Parsing failed and I don't know why"
  in
  let head_splt = String.split ~on:',' head_s in
  let head = List.map ~f:Int.of_string head_splt in
  let rec create_grids l acc bs =
    match l with
    | h :: t ->
      if String.(h = "")
      then
        create_grids t [ [] ]
          (Board.{ x_size = 5; y_size = 5; data = acc } :: bs)
      else create_grids t (parse_strings h :: acc) bs
    (*Final line*)
    | [] -> Board.{ x_size = 5; y_size = 5; data = acc } :: bs
  in
  (head, create_grids grids_s [ [] ] [])

let find_winning bs header =
  let rec aux winning bs nums =
    match winning with
    | true, Some b, num -> b, num
    | true, _, _ -> failwith "hmmmmm"
    | false, _, _ -> (
      match nums with
      | a :: b -> (
        let marked_bs = List.map bs ~f:(fun b -> Board.mark_by_value b a) in
        let res =
          List.fold ~init:None
            ~f:(fun acc x ->
              match Board.is_winner x with None -> acc | Some x -> Some x)
            marked_bs
        in
        match res with
        | Some x -> aux (true, Some x, a) marked_bs b
        | None -> aux (false, None, a) marked_bs b)
      | _ -> failwith "No winning board found!")
  in
  aux (false, None, 0) bs header

let () =
  let numbers, boards = parse "input" in
  printf "Header:\n";
  List.iter ~f:(fun x -> printf "%d " x) numbers;
  printf "\nBoards:\n";
  List.iter ~f:(fun x -> printf "%s\n" (Board.show x)) boards;
  (*Because I parse like a monkey*)
  let boards = List.rev (List.tl_exn (List.rev boards)) in
  let winning, last_called = find_winning boards numbers in
  let unmarked =
    Board.fold_board
      (fun m (v, b) -> if not b then (m + v) else m)
      0 winning
  in
  printf "Final score: %d\n" (unmarked * last_called)
