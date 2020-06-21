exception Unknown

type 'a tree =
  | Leaf
  | Node of 'a tree * 'a * 'a tree

let leaf   = Leaf
let node x = Node(leaf, x, leaf)

let rec min_value tree =
  match tree with
  | Leaf -> raise Unknown
  | Node(l,x,_) ->
    if l = Leaf then x else min_value l

let rec max_value tree =
  match tree with
  | Leaf -> raise Unknown
  | Node(_,x,r) ->
    if r = Leaf then x else max_value r

let rec insert value tree =
  match tree with
  | Leaf -> Node(Leaf, value, Leaf)
  | Node(l,x,r) as t ->
    if value < x then Node((insert value l), x, r)
    else if value > x then Node(l, x, (insert value r))
    (* Do nothing, duplicate key *)
    else t

(* Deletion in a binary search tree has three cases
 * 1.) node to be deleted is a leaf
 * 2.) node to be deleted has only one child
 * 3.) node to be deleted has two children
*)
let rec delete value tree =
  match tree with
  | Leaf -> tree
  | Node(l,x,r) ->
    if value = x then
      (* node to be deleted is a leaf *)
      if l = Leaf && r = Leaf then Leaf
      (* node to be deleted has only one child *)
      else if l = Leaf then r
      else if r = Leaf then l
      else
        (* find a minimum value in the right subtree *)
        (* replace value of the node to be removed with found minimum. Now, right subtree contains a duplicate! *)
        (* apply remove to the right subtree to remove a duplicate. *)
        let m = min_value r in
        let n = delete m r in Node(l,m,n)
    else if value < x then
      let lhs = delete value l in Node(lhs,x,r)
    else
      let rhs = delete value r in Node(l,x,rhs)

let rec member v tree =
  match tree with
  | Leaf -> false
  | Node(l,x,r) ->
    if v = x then true
    else
    if v < x then member v l
    else member v r

let rec height = function
  | Leaf -> 1
  | Node(l,_,r) -> max (1 + height l) (1 + height r)

(** Advanced traversals **)
let rec fold f acc tree =
  match tree with
  | Leaf -> acc
  | Node(l,x,r) -> f x (fold f acc l) (fold f acc r)

(** Construct a new tree from a list of values **)
let build xs =
  List.fold_left (fun x y -> insert y x) Leaf xs
