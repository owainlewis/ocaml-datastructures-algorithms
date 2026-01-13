(* Binary Search Tree Implementation *)

exception Empty_tree

type 'a tree =
  | Empty
  | Node of 'a tree * 'a * 'a tree

let empty = Empty
let node x = Node(Empty, x, Empty)

let rec min_value tree =
  match tree with
  | Empty -> raise Empty_tree
  | Node(l,x,_) -> if l = Empty then x else min_value l

let rec max_value tree =
  match tree with
  | Empty -> raise Empty_tree
  | Node(_,x,r) ->
    if r = Empty then x else max_value r

let rec insert value tree =
  match tree with
  | Empty -> Node(Empty, value, Empty)
  | Node(l,x,r) as t ->
    if value < x then Node((insert value l), x, r)
    else if value > x then Node(l, x, (insert value r))
    else t

(* Deletion in a binary search tree has three cases
 * 1.) node to be deleted is a leaf
 * 2.) node to be deleted has only one child
 * 3.) node to be deleted has two children
*)
let rec delete value tree =
  match tree with
  | Empty -> tree
  | Node(l,x,r) ->
    if value = x then
      if l = Empty && r = Empty then Empty
      else if l = Empty then r
      else if r = Empty then l
      else
        let m = min_value r in
        let n = delete m r in Node(l,m,n)
    else if value < x then
      let lhs = delete value l in Node(lhs,x,r)
    else
      let rhs = delete value r in Node(l,x,rhs)

let rec member v tree =
  match tree with
  | Empty -> false
  | Node(l,x,r) ->
    if v = x then true
    else if v < x then member v l
    else member v r

let rec height = function
  | Empty -> 0
  | Node(l,_,r) -> 1 + max (height l) (height r)

let rec fold f acc tree =
  match tree with
  | Empty -> acc
  | Node(l,x,r) -> f x (fold f acc l) (fold f acc r)

let rec inorder = function
  | Empty -> []
  | Node(l, x, r) -> inorder l @ [x] @ inorder r

let rec preorder = function
  | Empty -> []
  | Node(l, x, r) -> [x] @ preorder l @ preorder r

let rec postorder = function
  | Empty -> []
  | Node(l, x, r) -> postorder l @ postorder r @ [x]

let build xs =
  List.fold_left (fun t x -> insert x t) Empty xs
