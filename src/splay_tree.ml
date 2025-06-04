(* Splay Tree Implementation *)

(* A splay tree is a self-adjusting binary search tree with the
   property that recently accessed elements are quick to access again *)

type 'a tree =
  | Empty
  | Node of 'a tree * 'a * 'a tree

(* Right rotation *)
let right_rotate = function
  | Node(Node(a, x, b), y, c) -> Node(a, x, Node(b, y, c))
  | t -> t

(* Left rotation *)
let left_rotate = function
  | Node(a, x, Node(b, y, c)) -> Node(Node(a, x, b), y, c)
  | t -> t

(* Splay operation - brings the target key to the root *)
let rec splay key = function
  | Empty -> Empty
  | Node(l, k, r) as t ->
      if key = k then
        (* Found the key, return the tree *)
        t
      else if key < k then
        match l with
        | Empty -> t  (* Key not in tree *)
        | Node(ll, lk, lr) ->
            if key = lk then
              (* Zig: key is in left child, rotate right *)
              right_rotate t
            else if key < lk then
              (* Zig-Zig: key is in left-left grandchild *)
              match splay key ll with
              | Empty -> t
              | splayed -> right_rotate (right_rotate (Node(splayed, lk, lr)))
            else
              (* Zig-Zag: key is in left-right grandchild *)
              match splay key lr with
              | Empty -> t
              | splayed -> right_rotate (Node(ll, lk, Node(splayed, k, r)))
      else
        match r with
        | Empty -> t  (* Key not in tree *)
        | Node(rl, rk, rr) ->
            if key = rk then
              (* Zig: key is in right child, rotate left *)
              left_rotate t
            else if key > rk then
              (* Zig-Zig: key is in right-right grandchild *)
              match splay key rr with
              | Empty -> t
              | splayed -> left_rotate (left_rotate (Node(l, k, Node(rl, rk, splayed))))
            else
              (* Zig-Zag: key is in right-left grandchild *)
              match splay key rl with
              | Empty -> t
              | splayed -> left_rotate (Node(Node(l, k, splayed), rk, rr))

(* Insert a key into the splay tree *)
let insert key tree =
  let rec insert_bst k = function
    | Empty -> Node(Empty, k, Empty)
    | Node(l, k', r) as t ->
        if k = k' then t
        else if k < k' then Node(insert_bst k l, k', r)
        else Node(l, k', insert_bst k r)
  in
  
  let inserted = insert_bst key tree in
  splay key inserted

(* Find a key in the splay tree *)
let find key tree =
  let splayed = splay key tree in
  match splayed with
  | Empty -> (false, splayed)
  | Node(_, k, _) -> (k = key, splayed)

(* Delete a key from the splay tree *)
let delete key tree =
  let splayed = splay key tree in
  match splayed with
  | Empty -> Empty
  | Node(l, k, r) ->
      if k <> key then splayed
      else
        match l, r with
        | Empty, _ -> r
        | _, Empty -> l
        | _, _ ->
            (* Find the largest in the left subtree *)
            let max_l = splay max_int l in
            match max_l with
            | Empty -> failwith "Impossible: max_l is Empty"
            | Node(l', max_key, Empty) ->
                Node(l', max_key, r)
            | Node(l', max_key, r') ->
                failwith "Impossible: max_l has a right child"

(* Check if a key exists in the tree *)
let member key tree =
  let found, _ = find key tree in
  found

(* Build a splay tree from a list *)
let build xs =
  List.fold_left (fun t x -> insert x t) Empty xs

(* In-order traversal *)
let rec inorder = function
  | Empty -> []
  | Node(l, k, r) -> inorder l @ [k] @ inorder r

(* Visualization for debugging *)
let rec to_string_indented ?(indent=0) = function
  | Empty -> String.make indent ' ' ^ ".\n"
  | Node(l, k, r) ->
      let s = String.make indent ' ' ^ string_of_int k ^ "\n" in
      s ^ to_string_indented ~indent:(indent+2) l ^
      to_string_indented ~indent:(indent+2) r