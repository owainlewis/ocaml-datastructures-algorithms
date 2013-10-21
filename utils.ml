(* General utils *)

module type U = sig
  val range : int -> int -> int list
  val random_list : int -> int -> int list
end

module Utils : U =
struct
  let range = range
  let random_list = random_list
end

let range x y =
  let rec aux l item =
    let increment n = n+1 in
    if (item < y) then aux (l @ [item]) (increment item)
                  else l
  in aux [] x

(* An int array of random size in range r *)
let random_array size r = Array.init size (fun _ -> Random.int r)

let random_list size r =
  let l = ref [] in
  for i = 0 to size do
    l := Random.int r :: !l
  done;
  !l

let rec sum_recur = function
  |  [] -> 0
  | x::xs -> x + sum_recur xs

  (* no deferred operations on any recursive call *)
let sum lst =
  let rec aux' s a =
    match s with
      [] -> a
    | x::xs -> aux' xs (a+x) in
  aux' lst 0

let take_while ~f l =
  let rec aux ls = function
    | [] -> ls
    | x::xs -> if f x then aux (x::ls) xs else ls
  in
  List.rev (aux [] l)

(* [(|>)] is the forward pipe operator *)
let (|>) x f = f x 

(* Haskells compose . operator *)
let (>>) f g x = g (f x)

let flat_map f = List.concat >> List.map f

(* File reading utilty function *)
let read filename =
  let file_in = open_in filename in
  let rec aux acc =
    try aux (input_line file_in :: acc) with End_of_file -> close_in file_in; acc
  in
  aux [] |> List.filter ((<>) "") |> List.rev

let count_lines filename = filename |> read |> List.length

(* Take n lines from a given file *)
let take_lines n filename =
  let file_in = open_in filename in
  let rec aux acc count =
    if count = n then acc
    else 
      try let l = (input_line file_in :: acc) 
          in aux l (count+1)
      with End_of_file -> close_in file_in; acc
  in aux [] 0 |> List.rev
