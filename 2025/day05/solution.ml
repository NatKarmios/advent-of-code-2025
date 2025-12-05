module Tree = struct
  type t =
    | Leaf of bool
    | Node of t Array.t

  let with_next f = function
    | [] -> Leaf true
    | x::xs -> f (x, xs)

  let with_next2 f a b =
    match a, b with
    | [], [] -> Leaf true
    | (x::xs), (y::ys) -> f (x, xs) (y, ys)
    | _ -> failwith "bad"

  let rec make_bound is_upper (x, xs) =
    let f i =
      if i < x then Leaf is_upper
      else if i > x then Leaf (not is_upper)
      else with_next (make_bound is_upper) xs
    in
    Node (Array.init 10 f)

  let rec make_range (x, xs) (y, ys) =
    let f i =
      if i = x && i = y then
        with_next2 make_range xs ys
      else if i = x then
        with_next (make_bound false) xs
      else if i = y then
        with_next (make_bound true) ys
      else if x < i && i < y then
        Leaf true
      else Leaf false
    in
    Node (Array.init 10 f)

  let rec add_bound is_upper t (x, xs) =
    match t with
    | Leaf true -> Leaf true
    | Leaf false -> make_bound is_upper (x, xs)
    | Node n ->
      let n' = n |> Array.mapi @@ fun i t' ->
        if is_upper && i < x then
          Leaf true
        else if not is_upper && i > x then
          Leaf true
        else if i = x then
          with_next (add_bound is_upper t') xs
        else t'
      in
      Node n'

  let rec add_range t (x, xs) (y, ys) =
    match t with
    | Leaf true -> Leaf true
    | Leaf false -> make_range (x, xs) (y, ys)
    | Node n ->
      let n' = n |> Array.mapi @@ fun i t' ->
        if i = x && i = y then
          with_next2 (add_range t') xs ys
        else if i = x then
          with_next (add_bound false t') xs
        else if i = y then
          with_next (add_bound true t') ys
        else if x < i && i < y then
          Leaf true
        else t'
      in
      Node n'

  let build ranges =
    let rec aux acc = function
      | [] -> acc
      | (xs, ys)::rest ->
          let acc' = add_range acc xs ys in
          aux acc' rest
    in
    aux (Leaf false) ranges

  let rec check xs = function
    | Leaf b -> b
    | Node n ->
      match xs with
      | x'::xs' -> check xs' n.(x')
      | [] -> failwith "bad"

  let pow x n =
    let rec aux acc = function
    | 0 -> acc
    | n -> aux (x*acc) (n-1)
    in
    let a = aux 1 n in
    a

  let count digit =
    let rec aux acc digit = function
    | Leaf true -> acc + (pow 10 (digit))
    | Leaf false -> acc
    | Node n ->
        Array.fold_left (fun acc t -> aux acc (digit-1) t) acc n
    in
    aux 0 digit
end

let num_length = 16

let convert_num' x =
  let rec aux x = function
    | 0 -> []
    | n -> (x mod 10)::(aux (x / 10) (n-1))
  in
  List.rev (aux x num_length)

let convert_num x =
  let xs = convert_num' x in
  List.hd xs, List.tl xs

let read_ranges () =
  let lines = In_channel.(with_open_text "./input-ranges.txt" input_lines) in
  lines |> List.map @@ fun line ->
    match String.split_on_char '-' line with
    | [a; b] ->
        let xs = convert_num (int_of_string a) in
        let ys = convert_num (int_of_string b) in
        xs, ys
    | _ -> failwith ("Invalid line '" ^ line ^ "'")

let read_ids () =
  let lines = In_channel.(with_open_text "./input-ids.txt" input_lines) in
  lines |> List.map @@ fun line ->
    convert_num' (int_of_string line)

let read_input () =
  read_ranges (), read_ids ()

let main () =
  let ranges, ids = read_input () in
  let t = Tree.build ranges in
  let count = List.fold_left
    (fun acc id -> if Tree.check id t then acc+1 else acc)
    0 ids
  in
  Printf.printf "%d\n" count;
  let total = Tree.count num_length t in
  Printf.printf "%d\n" total


let () = main()

