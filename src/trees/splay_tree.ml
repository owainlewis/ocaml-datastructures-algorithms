(* Splay Tree Implementation *)

(* A splay tree is a self-adjusting binary search tree with the
   property that recently accessed elements are quick to access again *)

type 'a tree =
  | Empty
  | Node of 'a tree * 'a * 'a tree

(* Splay operation - brings the target key to the root *)
let rec splay key = function
  | Empty -> Empty
  | Node(l, k, r) as t ->
      if key = k then t
      else if key < k then
        match l with
        | Empty -> t  (* Key not in tree, return as is *)
        | Node(ll, lk, lr) ->
            if key = lk then
              (* Zig: rotate right *)
              Node(ll, lk, Node(lr, k, r))
            else if key < lk then
              (* Zig-Zig: splay in left-left, then double rotate right *)
              match splay key ll with
              | Empty -> Node(ll, lk, Node(lr, k, r))  (* Key not found *)
              | splayed_ll ->
                  Node(splayed_ll, lk, Node(lr, k, r)) |> function
                    | Node(Node(a, x, b), y, c) -> Node(a, x, Node(b, y, c))
                    | t -> t
            else
              (* Zig-Zag: splay in left-right, then rotate *)
              match splay key lr with
              | Empty -> Node(ll, lk, Node(lr, k, r))  (* Key not found *)
              | Node(lrl, lrk, lrr) ->
                  Node(Node(ll, lk, lrl), lrk, Node(lrr, k, r))
      else
        match r with
        | Empty -> t  (* Key not in tree, return as is *)
        | Node(rl, rk, rr) ->
            if key = rk then
              (* Zig: rotate left *)
              Node(Node(l, k, rl), rk, rr)
            else if key > rk then
              (* Zig-Zig: splay in right-right, then double rotate left *)
              match splay key rr with
              | Empty -> Node(Node(l, k, rl), rk, rr)  (* Key not found *)
              | splayed_rr ->
                  Node(Node(l, k, rl), rk, splayed_rr) |> function
                    | Node(a, x, Node(b, y, c)) -> Node(Node(a, x, b), y, c)
                    | t -> t
            else
              (* Zig-Zag: splay in right-left, then rotate *)
              match splay key rl with
              | Empty -> Node(Node(l, k, rl), rk, rr)  (* Key not found *)
              | Node(rll, rlk, rlr) ->
                  Node(Node(l, k, rll), rlk, Node(rlr, rk, rr))

(* Insert a key into the splay tree *)
let insert key tree =
  let rec insert_bst k = function
    | Empty -> Node(Empty, k, Empty)
    | Node(l, k', r) as t ->
        if k = k' then t
        else if k < k' then Node(insert_bst k l, k', r)
        else Node(l, k', insert_bst k r)
  in
  splay key (insert_bst key tree)

(* Find a key in the splay tree - returns (found, new_tree) *)
let find key tree =
  let splayed = splay key tree in
  match splayed with
  | Empty -> (false, splayed)
  | Node(_, k, _) -> (k = key, splayed)

(* Find the maximum key in a tree *)
let rec find_max = function
  | Empty -> None
  | Node(_, k, Empty) -> Some k
  | Node(_, _, r) -> find_max r

(* Delete a key from the splay tree *)
let delete key tree =
  let splayed = splay key tree in
  match splayed with
  | Empty -> Empty
  | Node(l, k, r) ->
      if k <> key then splayed  (* Key not found *)
      else
        match l with
        | Empty -> r
        | _ ->
            (* Splay the max of left subtree to root of left *)
            match find_max l with
            | None -> r
            | Some max_key ->
                let new_left = splay max_key l in
                match new_left with
                | Node(ll, lk, Empty) -> Node(ll, lk, r)
                | Node(ll, lk, _) -> Node(ll, lk, r)  (* Should not happen after splay *)
                | Empty -> r

(* Check if a key exists in the tree *)
let member key tree =
  let rec mem k = function
    | Empty -> false
    | Node(l, k', r) ->
        if k = k' then true
        else if k < k' then mem k l
        else mem k r
  in
  mem key tree

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
