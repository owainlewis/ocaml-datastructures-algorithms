(* Treap Implementation *)

(* A Treap is a randomized binary search tree where each node has both
   a key and a priority (randomly assigned). It maintains the BST property
   for keys and the heap property for priorities. *)

(* Random number generation for priorities *)
let rng = Random.State.make_self_init ()

type 'a treap =
  | Empty
  | Node of 'a * int * 'a treap * 'a treap  (* key, priority, left, right *)

(* Create a new node with a random priority *)
let make_node key left right =
  let priority = Random.State.int rng 100000 in
  Node(key, priority, left, right)

(* Create a node with a specified priority (for testing) *)
let make_node_with_priority key priority left right =
  Node(key, priority, left, right)

(* Right rotation *)
let rotate_right = function
  | Node(k2, p2, Node(k1, p1, a, b), c) ->
      Node(k1, p1, a, Node(k2, p2, b, c))
  | t -> t

(* Left rotation *)
let rotate_left = function
  | Node(k1, p1, a, Node(k2, p2, b, c)) ->
      Node(k2, p2, Node(k1, p1, a, b), c)
  | t -> t

(* Insert a key into the treap *)
let rec insert key = function
  | Empty -> make_node key Empty Empty
  | Node(k, p, left, right) as node ->
      if key < k then
        let new_left = insert key left in
        match new_left with
        | Node(k', p', _, _) when p' > p ->
            (* New node has higher priority, rotate right *)
            rotate_right (Node(k, p, new_left, right))
        | _ ->
            Node(k, p, new_left, right)
      else if key > k then
        let new_right = insert key right in
        match new_right with
        | Node(k', p', _, _) when p' > p ->
            (* New node has higher priority, rotate left *)
            rotate_left (Node(k, p, left, new_right))
        | _ ->
            Node(k, p, left, new_right)
      else
        (* Key already exists, just return the node *)
        node

(* Delete a key from the treap *)
let rec delete key = function
  | Empty -> Empty
  | Node(k, p, left, right) ->
      if key < k then
        Node(k, p, delete key left, right)
      else if key > k then
        Node(k, p, left, delete key right)
      else
        (* Found the node to delete *)
        merge left right

(* Merge two treaps *)
and merge left right =
  match left, right with
  | Empty, _ -> right
  | _, Empty -> left
  | Node(k1, p1, l1, r1), Node(k2, p2, l2, r2) ->
      if p1 > p2 then
        (* Left node has higher priority, becomes root *)
        Node(k1, p1, l1, merge r1 right)
      else
        (* Right node has higher priority, becomes root *)
        Node(k2, p2, merge left l2, r2)

(* Check if a key exists in the treap *)
let rec member key = function
  | Empty -> false
  | Node(k, _, left, right) ->
      if key = k then true
      else if key < k then member key left
      else member key right

(* Find the minimum key in the treap *)
let rec find_min = function
  | Empty -> failwith "Empty treap has no minimum"
  | Node(k, _, Empty, _) -> k
  | Node(_, _, left, _) -> find_min left

(* Find the maximum key in the treap *)
let rec find_max = function
  | Empty -> failwith "Empty treap has no maximum"
  | Node(k, _, _, Empty) -> k
  | Node(_, _, _, right) -> find_max right

(* Split the treap into two treaps: one with keys < x and one with keys >= x *)
let rec split x = function
  | Empty -> (Empty, Empty)
  | Node(k, p, left, right) ->
      if x < k then
        let l1, l2 = split x left in
        (l1, Node(k, p, l2, right))
      else
        let r1, r2 = split x right in
        (Node(k, p, left, r1), r2)

(* Tree traversals *)
let rec inorder = function
  | Empty -> []
  | Node(k, _, left, right) ->
      inorder left @ [k] @ inorder right

let rec preorder = function
  | Empty -> []
  | Node(k, _, left, right) ->
      [k] @ preorder left @ preorder right

(* Build a treap from a list *)
let build xs =
  List.fold_left (fun t x -> insert x t) Empty xs

(* Visualize the treap (for debugging) *)
let rec to_string_indented ?(indent=0) = function
  | Empty -> String.make indent ' ' ^ ".\n"
  | Node(k, p, left, right) ->
      let s = String.make indent ' ' ^ 
              Printf.sprintf "%d (p=%d)\n" k p in
      s ^ to_string_indented ~indent:(indent+2) left ^
      to_string_indented ~indent:(indent+2) right