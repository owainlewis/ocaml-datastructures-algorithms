(* BTree *)

module type TREE =
sig
  type 'a tree
  val insert : 'a -> 'a tree -> 'a tree
  val member : 'a -> 'a tree -> bool
  val size   : 'a -> int
end

module Binary =
struct

  type 'a tree =
    | Leaf
    | Node of 'a tree * 'a * 'a tree

  let empty = Leaf

  let rec insert v t =
    match t with
    | Leaf -> Node(Leaf, v, Leaf)
    | Node(l,x,r) ->
        if v <= x then
          Node((insert v l), x, r)
        else
          Node(l, x, (insert v r))

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

  let rec tree_fold f acc = function
    | Leaf -> acc
    | Node(l,x,r) -> f x (tree_fold f acc l) (tree_fold f acc r)

end
