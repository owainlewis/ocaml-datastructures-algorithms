(* Search Algorithms *)

(* Linear search - O(n) *)
let linear_search target lst =
  let rec aux idx = function
    | [] -> None
    | x :: xs ->
      if x = target then Some idx
      else aux (idx + 1) xs
  in
  aux 0 lst

(* Binary search on sorted array - O(log n) *)
let binary_search target arr =
  let rec aux low high =
    if low > high then None
    else
      let mid = (low + high) / 2 in
      let mid_val = arr.(mid) in
      if mid_val = target then Some mid
      else if mid_val < target then aux (mid + 1) high
      else aux low (mid - 1)
  in
  aux 0 (Array.length arr - 1)

(* Binary search on sorted list - O(log n) but with O(n) list access *)
let binary_search_list target lst =
  let arr = Array.of_list lst in
  binary_search target arr

(* Find first occurrence of element satisfying predicate *)
let find_first pred lst =
  let rec aux idx = function
    | [] -> None
    | x :: xs ->
      if pred x then Some (idx, x)
      else aux (idx + 1) xs
  in
  aux 0 lst

(* Find all occurrences of element *)
let find_all target lst =
  let rec aux idx acc = function
    | [] -> List.rev acc
    | x :: xs ->
      if x = target then aux (idx + 1) (idx :: acc) xs
      else aux (idx + 1) acc xs
  in
  aux 0 [] lst
