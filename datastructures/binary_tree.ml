(* BTree *)

type 'a tree = Leaf
	     | Node of 'a tree * 'a * 'a tree

module type BinaryTree = sig
    val insert : 'a -> 'a tree -> 'a tree
    val member : 'a -> 'a tree -> bool
    val size   : 'a -> int
  end

(* Binary Tree Insertion *)
let rec insert v tree =
  let build_node v = Node (Leaf, v, Leaf) in
  match tree with
    | Leaf          -> build_node v
    | Node(l, x, r) -> if v <= x then Node((insert v l), x, r)
                                 else Node(l, x, (insert v r))

(* Does the tree contain a value *)
let rec member v = function
  | Leaf          -> false
  | Node(l, x, r) ->
      if v = x then true
      else if v < x then member v l
      else member v r

(* Finds the height of a binary tree structure using recursion *)
let rec height = function
  | Leaf          -> 0
  | Node(l, x, r) -> max (1 + height l) (1 + height r)
