(* Sorting algorithms *)

(* Utility function to do the hard work *)

let rec insert item list =
  match list with
  | [] -> [item]
  | x::xs -> 
    match (item > x) with 
    | true -> x::(insert item xs)
    | false -> item::list

let rec insertion_sort list = 
  match list with
  | [] -> [] (* Already sorted *)
  | x::xs -> (insert x (insertionsort xs))

