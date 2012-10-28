(***************************************

 Binary Search Trees                 

***************************************)

module BTree =
  struct
  end

(* insert delete inorder searc min max *)
type 'a binary_tree =
  (* A root node and a left and right sub tree *)
  Node of 'a * 'a binary_tree * 'a binary_tree 
  (* Empty leaf *)
  | Leaf

(* Inserts an element into a binary tree *)
let rec insert element (tree : 'a binary_tree) : 'a binary_tree =
    match tree with
      Leaf -> Node(element, Leaf, Leaf)
    | Node(k, l, r) ->
        if element <= k then Node(k, (insert element l), r)
                        else Node(k, l, (insert element r))

(* Takes a list and constructs a binary tree from it.
   Sets the first element of the list to be the root node *)
let make_binary_tree (l : 'a list) : 'a binary_tree =
  let tree = insert (List.hd l) Leaf in
  let rec aux lst t =
    match lst with
      [] -> t
    | x :: xs -> aux xs (insert x t)
  in aux (List.tl l) tree

(* Binary tree traversal functions *)

let rec inorder = function
    Leaf -> []
  | Node(root, left, right) -> (inorder left) @ (root :: (inorder right))

