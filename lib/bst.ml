type 'a tree =
  | Leaf
  | Node of 'a tree * 'a * 'a tree

let empty = Leaf

let rec insert v t = match t with
  | Leaf -> Node(Leaf, v, Leaf)
  | Node(l,x,r) as t ->
    if v < x then Node((insert v l), x, r)
    else if v > x then Node(l, x, (insert v r))
    (* Do nothing, duplicate key *)
    else t 

(** TODO delete a node from the tree **)
let delete t = t
  
let rec min t = match t with
  | Leaf -> None
  | Node(l,x,_) ->
      if l = Leaf then Some(x) else min l

let rec member v = function
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
let rec fold f acc = function
  | Leaf -> acc
  | Node(l,x,r) -> f x (fold f acc l) (fold f acc r)

(** Examples **)
let example = insert 5 Leaf |> insert 4 |> insert 3
