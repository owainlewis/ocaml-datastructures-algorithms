(* A recursive insertion sort *)
module Sorting = struct
  (* Helper fn that inserts at the correct point in a list *)
  let rec insert v = function
    | [] -> [v] (* base case *)
    | x::xs as l -> if v < x then v :: l
			     else x :: (insert v xs)
  let rec insertion_sort = function
    | []    -> []
    | [x]   -> [x]
    | x::xs -> insert x (insertion_sort xs)
  let flip f x y = f y x
  (* A more elegant functional soltion is to do thie insertion sort using a fold *)
  (* In this example the insert function does all the work and we simply
     fold across the sequence moving each item into the correct place *)
  let insertion_with_fold l = List.fold_left (fun xs x -> insert x xs) [] l
end

let test () =
  let expected = [1;2;3;4;5] in
  let actual   = Sorting.insertion_with_fold [2;1;3;5;4] in
  if (expected = actual) then "PASS"
                         else "FAIL"
