(* Binary Trees *)

type 'a tree =
  | Leaf
  | Node of 'a tree * 'a * 'a tree

module type BTree = sig
  val insert : 'a -> 'a tree -> 'a tree
  val member : 'a -> 'a tree -> bool
  val size   : 'a tree -> int
end

type order = Lt | Eq | Gt

module Int =
struct
  type t = int
  let comp (x,y) =
  if x > y then Gt
  else if x < y then Lt
  else Eq
end

let rec insert v = function
  | Leaf -> Node(Leaf, v, Leaf)
  | Node(l,item,r)
     (* Insertion on the left subtree *)
      -> if v < item then Node((insert v l), item, r)
     (* Insertion on the right subtree *)
	 else if v > item then Node(l, item, (insert v r))
	 else Node(l, item, r)

(* Tree deletion *)
exception EmptyTree (* fail if calling delete on empty tree *)

let rec delete_max = function
  | Leaf -> raise EmptyTree
  | Node(l,v,Leaf) -> (v, l)
  | Node(l,v,r) ->
      let (max, r') = delete_max r in (max, Node(l,v,r'))

let rec delete x = function
  | Leaf -> Leaf
  | Node(l,v,Leaf) when v=x -> l
  | Node(Leaf,v,r) when v=x -> r
  | Node(l,v,r) ->
      if x=v then let (pred, l') = delete_max l in Node(l', pred, r)
      else if x < v then Node(delete x l, v, r)
      else Node(l,v, delete x r)

let rec contains value = function
  | Leaf -> false
  | Node(l,v,r) ->
      if value=v then true
		 else if value < v then contains value l
		 else contains value r

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

let rec preorder = function
    Leaf -> []
  | Node(l,v,r) -> [v] @ (preorder l) @ (preorder r)

let rec inorder = function
    Leaf -> []
  | Node(l,v,r) ->
      (inorder l) @ [v] @ (inorder r)

let rec postorder = function
    Leaf -> []
  | Node(l,v,r) -> postorder l @ postorder r @ [v]

(* Functions to map a function f over a tree structure *)
let rec pre_map ~f = function
  | Leaf -> []
  | Node(l,v,r) ->
     let x = f v in
     [x] @ (pre_map f l) @ (pre_map f r)

let rec inorder_map ~f = function
  | Leaf -> []
  | Node(l,v,r) ->
     let x = f v in
     (inorder_map f l) @ [x] @ (inorder_map f r)

let rec post_map ~f = function
  | Leaf -> []
  | Node(l,v,r) ->
     let x = f v in
     post_map f l @ post_map f r @ [x]

(* String Tree *)
let t = ['A';'B';'C';'D';'E';'F';'G';'H';'I'];;

(* Tree Functors fmap etc *)
