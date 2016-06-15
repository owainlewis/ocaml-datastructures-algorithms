module type SORTING =
  sig
    val sort : 'a list -> 'a list
  end

module SelectionSort : SORTING =
  struct
    let rec sort = function
    | [] -> []
    | h::t ->
	let rec aux y ys = function
        | []               -> y :: sort ys
	| x::xs when x < y -> aux x (y::ys) xs
	| x::xs            -> aux y (x::ys) xs
	in aux h [] t
  end

module Sort = struct
  let selection = SelectionSort.sort
end
