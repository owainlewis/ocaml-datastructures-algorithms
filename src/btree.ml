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

(* This makes more sense, putting the value in the middle *)

type 'a btree = Node of 'a btree * 'a * 'a btree | Leaf

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

let rec member e = function
    Leaf -> false
  | Node(v, l, r) -> 

(* Min value is the last item in the left sub tree *)

let rec min = function
  | Leaf -> 0
  | Node(v,l,_) -> v + (min l)

(* Find the height of a binary tree using recursion *)
let rec depth = function 
    | Leaf -> 0
    | Node(_,l,r) -> 1 + (max (depth l) (depth r))

(* Tail recursive version *)
let depth_tail_rec t =
  let rec aux depth = function
    | [] -> depth
    | (d, Leaf _) :: t -> aux (max d depth_tail_rec) t
    | (d, Node (_,left,right)) :: t ->
      let accu = (d+1, left) :: (d+1, right) :: t in
      aux depth accu in
  aux 0 [0, t]

(* Binary tree traversal functions *)

(* 

         1
        / \
       /   \
      /     \
     2       3
    / \     /
   4   5   6
  /       / \
 7       8   9

preorder:    1 2 4 7 5 3 6 8 9
inorder:     7 4 2 5 1 8 6 9 3
postorder:   7 4 5 2 8 9 6 3 1
level-order: 1 2 3 4 5 6 7 8 9

*)

let sample_tree =
  make_binary_tree [1;2;4;7;5;3;6;8;9]

let rec inorder = function
    Leaf -> []
  | Node(v,l,r) -> (inorder l) @ (v :: (inorder r))

let rec preorder = function
    Leaf -> []
  | Node(v,l,r) -> v :: ((preorder l) @ (preorder r))

