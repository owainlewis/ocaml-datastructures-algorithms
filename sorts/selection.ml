(* Functional selection sort *)

module type SELECTION =
  sig
    val selection_sort : 'a list -> 'a list
  end

module Sorting : SELECTION =
  struct
  let rec selection_sort = function
    | [] -> []
    | h::t ->
	let rec aux y ys = function
        | [] -> y :: selection_sort ys
	| x::xs when x < y -> aux x (y::ys) xs
	| x::xs            -> aux y (x::ys) xs
	in aux h [] t
  end
