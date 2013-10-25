(* Binary Trees *)

type 'a tree =
  | Leaf
  | Node of 'a tree * 'a * 'a tree

module type BTree = sig
  val insert : 'a -> 'a tree -> 'a tree
  val member : 'a -> 'a tree -> bool
  val size   : 'a tree -> int
end

let rec insert v = function
  | Leaf -> Node(Leaf, v, Leaf)
  | Node(l,item,r)
     (* Insertion on the left subtree *)
      -> if v < item then Node((insert v l), item, r)
     (* Insertion on the right subtree *)
         else if v > item then Node(l, item, (insert v r))
         else Node(l, item, r)

let rec contains value = function
  | Leaf -> false
  | Node(l,v,r) -> 
      if value=v then true
                 else if value < v then contains value l
                 else contains value r

(* Removal *)

let rec member x = function
  | Leaf -> false
  | Node(l,v,r) ->
      if x=v then true
      else if x < v then member x l
      else member x r

type direction = Left | Right

let height tree =
  let rec aux d = function
    | Leaf -> 0
    | Node(l,_,r) ->
        match d with
        | Left -> 1 + (aux Left l)
        | Right -> 1 + (aux Right r)
  in 
  let height_left = aux Left tree
  and height_right = aux Right tree
  in max height_left height_right

(* Build a tree from a list *)
let make_tree = 
  List.fold_left 
    (fun acc v -> insert v acc) Leaf

let root_node = function
  | Leaf -> None
  | Node(_,v,_) -> Some(v)

let rec left_sub_tree = function
  | Leaf -> []
  | Node(l,v,_) -> v :: left_sub_tree l

let rec right_sub_tree = function
  | Leaf -> []
  | Node(_,v,r) -> v :: right_sub_tree r

(* Traversals *)
(* *************************************************** *)

let sample_tree = make_tree [7;1;0;3;2;5;4;6;9;8;10];;

(* Pre-order *)
(* Visit the root *)
(* Traverse the left subtree *)
(* Traverse the right subtree *)
let rec preorder = function
    Leaf -> []
  | Node(l,v,r) -> [v] @ (preorder l) @ (preorder r)

(* In-order (symmetric) *)
(* Traverse the left subtree. *)
(* Visit the root. *)
(* Traverse the right subtree. *)
let rec inorder = function
    Leaf -> []
  | Node(l,v,r) -> 
      (inorder l) @ [v] @ (inorder r)

(* Post-order *)
(* Traverse the left subtree. *)
(* Traverse the right subtree. *)
(* Visit the root. *)
let rec postorder = function
    Leaf -> []
  | Node(l,v,r) -> postorder l @ postorder r @ [v]

(* Map a function over a binary tree in pre_order *)
let rec pre_map ~f = function
  | Leaf -> []
  | Node(l,v,r) -> 
     let x = f v in
     [x] @ (preorder l) @ (preorder r)

(* String Tree *)

let t = ['A';'B';'C';'D';'E';'F';'G';'H';'I'];;

(* Tree Functors fmap etc *)

