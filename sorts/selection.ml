(* Functional selection sort *)

let rec selection_sort = function
  | [] -> []
  | h::t ->
      let rec aux y ys = function
        [] -> y :: selection_sort ys
      | x::xs when x < y -> aux x (y::ys) xs
      | x::xs            -> aux y (x::ys) xs
      in aux h [] t

(* Imp selection sort example *)
