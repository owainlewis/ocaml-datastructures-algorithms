(* Sorting algorithms *)

(* INSERTION *)

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
  | x::xs -> (insert x (insertion_sort xs))

let insertion_sort_foldable lst = 
  List.fold_right insert lst []

(* MERGE *)

let rec split_at n xs =
  match n, xs with
    | 0, xs -> [], xs
    | _, [] ->
        failwith "index too large"
    | n, x::xs when n > 0 ->
        let xs', xs'' = split_at (pred n) xs in
          x::xs', xs''
    | _, _ ->
        invalid_arg "negative argument"
 
let rec merge_sort cmp = function
  | []  -> []
  | [x] -> [x]
  | xs  -> 
      let xs, ys = split_at (List.length xs / 2) xs in
      List.merge cmp (merge_sort cmp xs) (merge_sort cmp ys)

(* QUICK *)

let rec quicksort gt = function
  | [] -> []
  | x::xs ->
      let ys, zs = List.partition (gt x) xs in
      (quicksort gt ys) @ (x :: (quicksort gt zs))

