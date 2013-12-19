(* OCaml Binary Search Trees *)

type comparison = Less
		| Equal
		| Greater

module type ORDERED_TYPE =
  sig
    type t
    val cmp: t -> t -> comparison
  end

module BinarySearchTree =
  functor (Elt: ORDERED_TYPE) ->
    struct
      type t = Elt.t
      type bst = Empty
	       | Node of t * bst * bst
      let empty() = Empty
      let is_empty = function
	Empty          -> true
      | Node (_, _, _) -> false
      let rec insert item = function
	Empty -> Node (item, Empty, Empty)
      | Node (m, left, right) ->
	  match Elt.cmp item m with
	    Equal   -> Node (m, left, right)
	  | Less    -> Node (m, (insert item left), right)
	  | Greater -> Node (m, left, (insert item right))
      let rec min = function
	Empty -> None
      | Node (_, left, _) ->
	  match left with
	    Empty -> None
	  | Node(v, Empty, Empty) -> Some(v)
	  | Node(_, l, _) -> min(l)
      end

module OrderedIntType =
  struct
    type t = int
    let cmp x y = if x = y then Equal
		  else if x < y then Less
		  else Greater
  end

module IntBst = BinarySearchTree(OrderedIntType)
