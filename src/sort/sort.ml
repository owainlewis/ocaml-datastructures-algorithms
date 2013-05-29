(*********************************

  Sorting algorithms

  insertion
  selection
  merge
  bubble
  quick
  shell

********************************)

(* Insertion sort *)
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

(* Merge sort *)
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

(* Quick sort *)
let rec quicksort gt = function
  | [] -> []
  | x::xs ->
      let ys, zs = List.partition (gt x) xs in
      (quicksort gt ys) @ (x :: (quicksort gt zs))

let n = [5;1;4;2;8]

(* functional ish bubble sort using recursion *)
let rec bubblesort l =
  let rec aux = function
    | x::y::xs -> if x > y then y :: aux(x::xs)
                           else x :: aux(y::xs)
    | x :: xs -> x :: aux xs
    | [] -> []
  in
  let p = aux l in
  if l <> p then bubblesort p (* do another sweep *)
            else l

