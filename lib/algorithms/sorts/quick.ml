(* Quick sort *)
module type QUICK =
  sig
    val quick_sort : 'a list -> 'a list
  end

module Sorting : QUICK = struct
  let rec quick_sort = function
    | [] -> []
    | x::xs -> let smaller, larger = List.partition (fun y -> y < x) xs
               in let x = (quick_sort smaller)
		  and y = (x::quick_sort larger)
		  in x @ y
end
