(* General utils *)

open Core.Std

module type U = sig
  val range : int -> int -> int list
  val random_list : int -> int -> int list
end

let range x y =
  let rec aux l item =
    let increment n = n+1 in
    if (item < y) then aux (l @ [item]) (increment item)
		  else l
  in aux [] x

let map_with_index ~f l =
  let rec aux i fn lst r = match lst with
    | []    -> List.rev r
    | x::xs -> aux (i+1) fn xs ((i, fn(x)) :: r)
  in aux 0 f l []

(* An int array of random size in range r *)
let random_array size r = Array.init size (fun _ -> Random.int r)

let random_list size r =
  let l = ref [] in
  for i = 0 to size do
    l := Random.int r :: !l
  done;
  !l

module Utils : U =
struct
  let range = range
  let random_list = random_list
end

let rec sum_recur = function
  | [] -> 0
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
  List.rev (aux [] (* [(|>)] is the forward pipe operator *)

let (|>) x f = f x

(* Haskells compose . operator *)

let (>>) f g x = g (f x)

let flat_map f = List.concat >> List.map f

let print   = print_string
let println = print_endline

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

(* Apply a function to each line in a channel *)
(* e.g each_line (open_in "README.md") print_endline *)
let rec do_each_line channel f =
  try
    let line = input_line channel in
    f line;
    do_each_line channel f
  with
    | End_of_file -> close_in channel
    | _ -> failwith "Exception raised while processing file"

let map_lines file f =
  do_each_line (open_in file) f

let sum_squares xs =
     xs
  |> List.fold_left (fun acc v -> (v*v)::acc) []
  |> sum

let rec zip v1 v2 =
  match (v1, v2) with
  | [], _ -> []
  | _, [] -> []
  | (x::xs), (y::ys) -> (x,y)::zip xs ys

let dot_product v1 v2 =
  let zipped = zip v1 v2 in
  List.map (fun (x,y) -> x * y) zipped |> sum

type 'a point = Point of 'a * 'a

(* Euclidean distance for two points *)
let euclidean p1 p2 =
  match (p1,p2) with
  | P(x1,x2), P(y1,y2) ->
      let a = x1-y1 in
      let b = x2-y2 in
      let c = (a*a) + (b*b) in sqrt (float_of_int c)
  | _              -> failwith "Arguments p1 and p2 be of type P(x,y)"

(* List Utils *)

let map_squares =
  List.map ~f:(fun x -> x*x)

let rec forall p l =
  match l with
    [] -> true
  | h::t -> p(h) & forall p t;;

let list_empty = function
  | [] -> true
  | _  -> false

let rec filter p = function
  | []    -> []
  | x::xs -> if p x then x::filter p xs else filter p xs

let multiple_of n x = x mod n = 0

let remove_multiples_of n =
  in filter (fun v -> v mod n <> 0)
