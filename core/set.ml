(* OCaml Set *)

type compare_to = Lt | Eq | Gt

module type ORDERED_TYPE =
  sig
    type t
    val cmp: t -> t -> compare_to
  end

module type SET_TYPE = sig
end

module OrderedString =
  struct
    type t = string
    let cmp x y = if x = y then Eq else if x < y then Lt else Gt
  end

module OrderedInt =
  struct
    type t = int
    let cmp x y = if x = y then Eq else if x < y then Lt else Gt
  end

module OSet = functor (Elt: ORDERED_TYPE) ->
  struct
    type elem = Elt.t
    type set  = elem list
    let empty = []
    (* Invariant :- keep ordering of elements at all times *)
    let rec add x oset =
      match oset with
	[] -> [x]
      | y::ys ->
	  (* Compare x = y *)
	  match Elt.cmp x y with
	    Eq -> oset (* Element already exists so just return the existing set *)
	  | Lt -> x :: oset (* Insert the element at hd position *)
	  | Gt -> y :: add x ys (* shuffle up and insert in the correct place *)
    let rec contains_element x = function
	[] -> false
      | y::ys ->
	  (* Compare x = y *)
	  match Elt.cmp x y with
	    Eq -> true
	  | _  -> contains_element x ys
    let rec member x = function
	[] -> false
      | y::ys ->
	  (* Compare x = y *)
	  match Elt.cmp x y with
	    Eq -> true
	  | Lt -> false (* invariant x can't be less than s@(y::ys) so does not exist *)
	  | Gt -> member x ys (* recur *)
  end

module OStringSet = OSet(OrderedString)
module OIntSet    = OSet(OrderedInt)
