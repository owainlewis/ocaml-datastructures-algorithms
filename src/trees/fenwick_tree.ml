(* Fenwick Tree (Binary Indexed Tree) Implementation *)

(* A Fenwick Tree or Binary Indexed Tree is a data structure that can efficiently
   update elements and calculate prefix sums in a table of numbers. *)

type t = {
  tree: int array;  (* 1-based indexing *)
  size: int;        (* Size of the original array *)
}

(* Create a new Fenwick tree from an array *)
let create arr =
  let n = Array.length arr in
  let tree = Array.make (n + 1) 0 in
  
  (* Build the tree *)
  for i = 0 to n - 1 do
    (* 1-based indexing *)
    let idx = i + 1 in
    tree.(idx) <- arr.(i);
    
    (* Update all responsible ancestors *)
    let parent = idx + (idx land (-idx)) in
    if parent <= n then
      tree.(parent) <- tree.(parent) + tree.(idx)
  done;
  
  { tree; size = n }

(* Create an empty Fenwick tree of given size *)
let create_empty size =
  { tree = Array.make (size + 1) 0; size }

(* Get the sum of the first i elements (0-based index) *)
let prefix_sum ft i =
  if i < 0 || i >= ft.size then
    failwith "Index out of bounds";
  
  let i = i + 1 in (* Convert to 1-based *)
  let rec sum idx acc =
    if idx = 0 then acc
    else sum (idx - (idx land (-idx))) (acc + ft.tree.(idx))
  in
  sum i 0

(* Get the sum of elements in range [start, end] (inclusive, 0-based) *)
let range_sum ft start_idx end_idx =
  if start_idx < 0 || end_idx >= ft.size || start_idx > end_idx then
    failwith "Invalid range";
  
  if start_idx = 0 then
    prefix_sum ft end_idx
  else
    prefix_sum ft end_idx - prefix_sum ft (start_idx - 1)

(* Update the value at index i by adding delta (0-based index) *)
let update ft i delta =
  if i < 0 || i >= ft.size then
    failwith "Index out of bounds";
  
  let i = i + 1 in (* Convert to 1-based *)
  let rec update_aux idx =
    if idx <= ft.size then (
      ft.tree.(idx) <- ft.tree.(idx) + delta;
      update_aux (idx + (idx land (-idx)))
    )
  in
  update_aux i

(* Set the value at index i to val (0-based index) *)
let set ft i new_val =
  if i < 0 || i >= ft.size then
    failwith "Index out of bounds";
  
  let current = 
    if i = 0 then prefix_sum ft 0
    else prefix_sum ft i - prefix_sum ft (i - 1)
  in
  update ft i (new_val - current)

(* Get the value at index i (0-based index) *)
let get ft i =
  if i < 0 || i >= ft.size then
    failwith "Index out of bounds";
  
  if i = 0 then
    prefix_sum ft 0
  else
    prefix_sum ft i - prefix_sum ft (i - 1)

(* Create a Fenwick tree from a list *)
let of_list lst =
  create (Array.of_list lst)

(* Convert a Fenwick tree to a list of original values *)
let to_list ft =
  let result = Array.make ft.size 0 in
  for i = 0 to ft.size - 1 do
    result.(i) <- get ft i
  done;
  Array.to_list result

(* Find the largest index with prefix sum less than or equal to target *)
let find_largest_with_sum_leq ft target =
  let rec binary_search left right =
    if left > right then
      if left > 0 then left - 1 else -1
    else
      let mid = left + (right - left) / 2 in
      let sum = prefix_sum ft mid in
      if sum <= target then
        binary_search (mid + 1) right
      else
        binary_search left (mid - 1)
  in
  binary_search 0 (ft.size - 1)