(***************************************

 Binary Search Trees

 - insert 
 - member - boolean
 - delete
 - get
 - height 
 - min
 - max  

***************************************)

type 'a tree =
  | Leaf
  | Node of 'a tree * 'a * 'a tree

let rec insert v = function
  | Leaf -> Node(Leaf, v, Leaf)
  | Node(l, x, r) ->
      if v < x then Node(insert v l, x, r)
      else if v > x then Node(l, x, insert v r)
      else Node(l, x, r)

let build values =
  let rec aux v tree =
    match v with
    | [] -> tree
    | x :: xs -> aux xs (insert x tree) in
  aux values 

let rec member v = function
  | Leaf -> false
  | Node(l, x, r) ->
    if v == x then true
    else if v < x then (member v l)
    else (member v r)

(* Min value is the last item in the left sub tree *)
let min_value = function
  | Leaf -> failwith "Empty tree"
  | Node(l, x, _) ->
    let rec aux t m =
      match t with
      | Leaf -> m
      | Node(l, x, r) -> aux l x
    in aux l x 

(* Max value is the last item in the right sub tree *)
let max_value = function
  | Leaf -> failwith "Empty tree"
  | Node(_, x, r) ->
    let rec aux t m =
      match t with
      | Leaf -> m
      | Node(_, x, r) -> aux r x
    in aux r x

(* 
 * let test_tree = build [1;2;3;4;5]
 * find 2 (test_tree Leaf) 
 *)

module BTree =
  struct
    (* This makes more sense, putting the value in the middle *)
    type 'a btree = 
      | Node of 'a btree * 'a * 'a btree 
      | Leaf
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
    | Leaf -> Node(element, Leaf, Leaf)
    | Node(k, l, r) ->
        if element <= k then Node(k, (insert element l), r)
                        else Node(k, l, (insert element r))

(* Takes a list and constructs a binary tree from it.
   Sets the first element of the list to be the root node *)
let make_binary_tree (l : 'a list) : 'a binary_tree =
  let tree = insert (List.hd l) Leaf in
  let rec aux lst t =
    match lst with
    |  [] -> t
    | x :: xs -> aux xs (insert x t)
  in aux (List.tl l) tree

(* Find the height of a binary tree using recursion *)
let rec depth = function 
    | Leaf -> 0
    | Node(_,l,r) -> 1 + (max (depth l) (depth r))

(*****************************

preorder:    1 2 4 7 5 3 6 8 9
inorder:     7 4 2 5 1 8 6 9 3
postorder:   7 4 5 2 8 9 6 3 1
level-order: 1 2 3 4 5 6 7 8 9

*****************************)

let sample_tree =
  make_binary_tree [1;2;4;7;5;3;6;8;9]

let rec inorder = function
    Leaf -> []
  | Node(v,l,r) -> (inorder l) @ (v :: (inorder r))

let rec preorder = function
    Leaf -> []
  | Node(v,l,r) -> v :: ((preorder l) @ (preorder r))

