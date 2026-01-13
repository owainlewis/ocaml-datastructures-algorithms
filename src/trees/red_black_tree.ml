(* Red Black Tree Implementation *)

(* Red-Black trees maintain the following invariants:
   1. Every node is either red or black
   2. The root is always black
   3. Red nodes can only have black children
   4. Every path from root to leaf has the same number of black nodes
*)

type color = Red | Black

type 'a tree =
  | Empty
  | Node of color * 'a * 'a tree * 'a tree

let empty = Empty

let rec member x tree =
  match tree with
  | Empty -> false
  | Node (_, value, left, right) ->
    if x = value then true
    else if x < value then member x left
    else member x right

(* Balance after insertion - handles all four rotation cases *)
let balance = function
  | Black, z, Node (Red, y, Node (Red, x, a, b), c), d
  | Black, z, Node (Red, x, a, Node (Red, y, b, c)), d
  | Black, x, a, Node (Red, z, Node (Red, y, b, c), d)
  | Black, x, a, Node (Red, y, b, Node (Red, z, c, d)) ->
    Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
  | color, value, left, right ->
    Node (color, value, left, right)

let insert x s =
  let rec ins = function
    | Empty -> Node (Red, x, Empty, Empty)
    | Node (color, y, a, b) as s ->
      if x < y then balance (color, y, ins a, b)
      else if x > y then balance (color, y, a, ins b)
      else s
  in
  match ins s with
  | Node (_, y, a, b) -> Node (Black, y, a, b)
  | Empty -> failwith "insert: impossible - ins always returns Node"

(* Delete operation for Red-Black trees *)

(* Helper to check if a node is black (Empty counts as black) *)
let is_black = function
  | Empty -> true
  | Node (Black, _, _, _) -> true
  | _ -> false

(* Find minimum value in a tree *)
let rec min_value = function
  | Empty -> failwith "min_value: empty tree"
  | Node (_, x, Empty, _) -> x
  | Node (_, _, left, _) -> min_value left

(* Balance for deletion - reuse the standard balance function *)
let balance_delete (color, value, left, right) =
  balance (color, value, left, right)

let rec delete x = function
  | Empty -> Empty
  | Node (color, y, left, right) ->
    if x < y then
      balance_delete (color, y, delete x left, right)
    else if x > y then
      balance_delete (color, y, left, delete x right)
    else
      (* Found the node to delete *)
      match left, right with
      | Empty, Empty -> Empty
      | Empty, child | child, Empty -> child
      | _, _ ->
        (* Node has two children - replace with successor *)
        let successor = min_value right in
        balance_delete (color, successor, left, delete successor right)

(* Tree traversals *)
let rec inorder = function
  | Empty -> []
  | Node (_, value, left, right) ->
    inorder left @ [value] @ inorder right

let rec preorder = function
  | Empty -> []
  | Node (_, value, left, right) ->
    [value] @ preorder left @ preorder right

let rec postorder = function
  | Empty -> []
  | Node (_, value, left, right) ->
    postorder left @ postorder right @ [value]

(* Build tree from a list *)
let build xs =
  List.fold_left (fun t x -> insert x t) Empty xs

(* Get the height of the tree *)
let rec height = function
  | Empty -> 0
  | Node (_, _, left, right) -> 1 + max (height left) (height right)

(* Get the black height (number of black nodes to any leaf) *)
let rec black_height = function
  | Empty -> 1
  | Node (Black, _, left, _) -> 1 + black_height left
  | Node (Red, _, left, _) -> black_height left
