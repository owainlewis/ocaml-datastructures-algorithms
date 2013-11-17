(* Sorting algorithms functional and imp *)

open Utils

module type SORTSIG = 
  sig
    val selection_sort : 'a list -> 'a list
    val insertion_sort : 'a list -> 'a list
    val bubble_sort    : 'a list -> 'a list
  end

module SortAlgorithms : SORTSIG = struct
  let rec selection_sort = function
      [] -> []
    | h::t ->
	let rec aux y ys = function
	    [] -> y :: selection_sort ys
	  | x::xs when x < y -> aux x (y::ys) xs
	  | x::xs            -> aux y (x::ys) xs
	in
	aux h [] t
  (* Insertion sort using an auxilary insertion helper *)
  let rec insertion_sort lst =
    let rec insert v = function
	[] -> [v]
      | x::xs as l -> if v < x then v :: l else x :: (insert v xs) 
    in 
    match lst with
	[]    -> []
      | [x]   -> [x]
      | x::xs -> insert x (insertion_sort xs)

  let rec bubble_sort lst =
    let rec aux = function
      | [] -> []
      | x::y::xs -> 
         if x > y then y::aux(x::xs)
                  else x::aux(y::xs)
      | x::xs -> x :: aux xs
    in let p = aux lst in
    if lst <> p then bubble_sort p
                else lst
end

(** Utils **)
let rec swap (l, n) =
  let rec loop xs count acc =
    match xs with
    | _ when count = n -> xs @ List.rev acc
    | [] -> List.rev acc
    | h::t -> loop t (count+1) (h::acc)
   in loop l 0 []
(* Mutable state for array swap *)
let array_swap xs i j =
  let temp = xs.(i) in
  xs.(i) <- xs.(j);
  xs.(j) <- temp

let selection_sort_array_mutable xs =
  let swap a i j =
    let temp = a.(i) in
    a.(i) <- a.(j); a.(j) <- temp
  and find_min arr i =
    let m = ref i in
    for j=i+1 to Array.length arr -1 do
      let mval = !m in
      if xs.(j) < xs.(mval) then m := j
    done;
    !m
  in
  let r = ref [] in
  for i = 0 to Array.length xs - 2 do
    let min = find_min xs i in
    swap xs i min
  done;
  xs

(* Tests *)
(************************************)

let time f x =
  let t = Sys.time() in
  let fx = f x in
  Printf.printf "Execution time: %fs\n" (Sys.time() -. t)

let tests = [
  SortAlgorithms.selection_sort;
  SortAlgorithms.insertion_sort;
  SortAlgorithms.bubble_sort;
]

let run tests = 
  let generic_sort = List.sort (fun x y -> if x > y then 1 else 0)
  in
  let passed = ref 0
  and failed = ref 0 in
  let unsorted = Utils.random_list 1000 1000 in
  List.map (fun f -> if (f unsorted = generic_sort unsorted) 
                     then incr passed 
                     else incr failed) tests;
  Printf.printf "\n\nPassed %d Failed %d\n\n" !passed !failed

let main() = run tests;;
  
let time_all() =
  let unsorted = Utils.random_list 5000 5000 in
  Printf.printf "Selection sort -> ";
  time SortAlgorithms.selection_sort unsorted;
  Printf.printf "Insertion sort -> ";
  time SortAlgorithms.insertion_sort unsorted;
  Printf.printf "Bubble sort -> ";
  time SortAlgorithms.bubble_sort unsorted

