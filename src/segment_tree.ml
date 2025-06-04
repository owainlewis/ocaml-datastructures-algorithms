(* Segment Tree Implementation *)

(* A segment tree for range queries and updates *)
type 'a t = {
  size: int;                (* Size of the original array *)
  tree: 'a array;           (* The actual tree storage *)
  combine: 'a -> 'a -> 'a;  (* Function to combine values *)
  default: 'a;              (* Default/identity value *)
}

(* Calculate the size needed for the tree array *)
let calc_size n =
  let rec next_pow2 x =
    if x >= n then x else next_pow2 (x * 2)
  in
  2 * next_pow2 1 - 1

(* Build a segment tree from an array *)
let build arr combine default =
  let n = Array.length arr in
  if n = 0 then
    { size = 0; tree = [||]; combine; default }
  else
    let tree_size = calc_size n in
    let tree = Array.make tree_size default in
    
    (* Recursive build function *)
    let rec build_rec arr_idx left right tree_idx =
      if left = right then
        (* Leaf node *)
        tree.(tree_idx) <- arr.(left)
      else
        (* Internal node *)
        let mid = left + (right - left) / 2 in
        let left_child = 2 * tree_idx + 1 in
        let right_child = 2 * tree_idx + 2 in
        
        build_rec arr_idx left mid left_child;
        build_rec arr_idx (mid + 1) right right_child;
        
        tree.(tree_idx) <- combine tree.(left_child) tree.(right_child)
    in
    
    build_rec arr 0 (n - 1) 0;
    { size = n; tree; combine; default }

(* Query a range [start, end] *)
let query seg_tree query_start query_end =
  if query_start < 0 || query_end >= seg_tree.size || query_start > query_end then
    failwith "Invalid query range";
  
  let rec query_rec left right tree_idx =
    (* Complete overlap *)
    if query_start <= left && query_end >= right then
      seg_tree.tree.(tree_idx)
    (* No overlap *)
    else if query_start > right || query_end < left then
      seg_tree.default
    (* Partial overlap - recurse on children *)
    else
      let mid = left + (right - left) / 2 in
      let left_child = 2 * tree_idx + 1 in
      let right_child = 2 * tree_idx + 2 in
      
      let left_result = query_rec left mid left_child in
      let right_result = query_rec (mid + 1) right right_child in
      
      seg_tree.combine left_result right_result
  in
  
  query_rec 0 (seg_tree.size - 1) 0

(* Update a value at a specific index *)
let update seg_tree idx new_value =
  if idx < 0 || idx >= seg_tree.size then
    failwith "Index out of bounds";
  
  let rec update_rec left right tree_idx =
    if left = right then
      (* Leaf node - update it *)
      seg_tree.tree.(tree_idx) <- new_value
    else
      (* Internal node - recurse on appropriate child *)
      let mid = left + (right - left) / 2 in
      let left_child = 2 * tree_idx + 1 in
      let right_child = 2 * tree_idx + 2 in
      
      if idx <= mid then
        update_rec left mid left_child
      else
        update_rec (mid + 1) right right_child;
      
      (* Recompute this node's value *)
      seg_tree.tree.(tree_idx) <- seg_tree.combine 
        seg_tree.tree.(left_child) 
        seg_tree.tree.(right_child)
  in
  
  update_rec 0 (seg_tree.size - 1) 0;
  seg_tree

(* Create a segment tree for range sum queries *)
let sum_segment_tree arr =
  build arr (+) 0

(* Create a segment tree for range minimum queries *)
let min_segment_tree arr =
  build arr min max_int

(* Create a segment tree for range maximum queries *)
let max_segment_tree arr =
  build arr max min_int

(* Create a segment tree for range GCD queries *)
let gcd_segment_tree arr =
  let rec gcd a b =
    if b = 0 then abs a else gcd b (a mod b)
  in
  build arr gcd 0

(* Create a segment tree for range product queries *)
let product_segment_tree arr =
  build arr ( * ) 1