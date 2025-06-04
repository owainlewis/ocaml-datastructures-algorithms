(* AVL Tree Implementation *)

type 'a tree =
  | Empty
  | Node of 'a * 'a tree * 'a tree * int (* value, left, right, height *)

(* Get height of a tree *)
let height = function
  | Empty -> 0
  | Node(_, _, _, h) -> h

(* Create a new node with proper height *)
let make_node value left right =
  let h = 1 + max (height left) (height right) in
  Node(value, left, right, h)

(* Get balance factor of a node *)
let balance_factor = function
  | Empty -> 0
  | Node(_, left, right, _) -> height left - height right

(* Rotate right *)
let rotate_right = function
  | Node(k2, Node(k1, a, b, _), c, _) ->
      make_node k1 a (make_node k2 b c)
  | _ -> failwith "Cannot rotate right"

(* Rotate left *)
let rotate_left = function
  | Node(k1, a, Node(k2, b, c, _), _) ->
      make_node k2 (make_node k1 a b) c
  | _ -> failwith "Cannot rotate left"

(* Balance a node *)
let balance node =
  match node with
  | Empty -> Empty
  | Node(value, left, right, _) as n ->
      let bf = balance_factor n in
      if bf > 1 then
        (* Left heavy *)
        if balance_factor left >= 0 then
          (* Left-Left case *)
          rotate_right n
        else
          (* Left-Right case *)
          let left' = rotate_left left in
          rotate_right (make_node value left' right)
      else if bf < -1 then
        (* Right heavy *)
        if balance_factor right <= 0 then
          (* Right-Right case *)
          rotate_left n
        else
          (* Right-Left case *)
          let right' = rotate_right right in
          rotate_left (make_node value left right')
      else
        (* Already balanced *)
        make_node value left right

(* Insert a value into the AVL tree *)
let rec insert value = function
  | Empty -> Node(value, Empty, Empty, 1)
  | Node(v, left, right, _) as node ->
      if value < v then
        balance (make_node v (insert value left) right)
      else if value > v then
        balance (make_node v left (insert value right))
      else
        (* Duplicate keys not allowed *)
        node

(* Find minimum value in a tree *)
let rec find_min = function
  | Empty -> failwith "Empty tree has no minimum"
  | Node(value, Empty, _, _) -> value
  | Node(_, left, _, _) -> find_min left

(* Delete a value from the AVL tree *)
let rec delete value = function
  | Empty -> Empty
  | Node(v, left, right, _) ->
      if value < v then
        balance (make_node v (delete value left) right)
      else if value > v then
        balance (make_node v left (delete value right))
      else
        (* Node to delete found *)
        match left, right with
        | Empty, Empty -> Empty
        | Empty, _ -> right
        | _, Empty -> left
        | _, _ ->
            (* Node with two children *)
            let min_val = find_min right in
            balance (make_node min_val left (delete min_val right))

(* Check if a value exists in the tree *)
let rec member value = function
  | Empty -> false
  | Node(v, left, right, _) ->
      if value = v then true
      else if value < v then member value left
      else member value right

(* Tree traversals *)
let rec inorder = function
  | Empty -> []
  | Node(value, left, right, _) ->
      inorder left @ [value] @ inorder right

let rec preorder = function
  | Empty -> []
  | Node(value, left, right, _) ->
      [value] @ preorder left @ preorder right

let rec postorder = function
  | Empty -> []
  | Node(value, left, right, _) ->
      postorder left @ postorder right @ [value]

(* Build tree from a list *)
let build xs =
  List.fold_left (fun tree x -> insert x tree) Empty xs

(* Visualize tree structure (for debugging) *)
let rec to_string_indented ?(indent=0) = function
  | Empty -> String.make indent ' ' ^ ".\n"
  | Node(value, left, right, h) ->
      let s = String.make indent ' ' ^ 
              Printf.sprintf "%d (h=%d, bf=%d)\n" 
                value h (balance_factor (Node(value, left, right, h))) in
      s ^ to_string_indented ~indent:(indent+2) left ^
      to_string_indented ~indent:(indent+2) right