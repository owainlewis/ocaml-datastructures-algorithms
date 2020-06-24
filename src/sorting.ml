module Algorithms =
struct
  let rec bubble: 'a list -> 'a list = fun xs ->
    let rec aux ys = match ys with
      | []    -> []
      | x::[] -> [x]
      | x::y::xs when x > y -> y::aux(x::xs)
      | x::y::xs -> x :: aux(y::xs)
    in
    let sweep = aux xs in
    if sweep = xs
    then sweep
    else bubble sweep

  let rec selection_sort = function
    | [] -> []
    | h::t ->
      let rec aux y ys = function
        | [] -> y :: selection_sort ys
	| x::xs when x < y -> aux x (y::ys) xs
	| x::xs            -> aux y (x::ys) xs
      in aux h [] t

  let rec insertion xs =
    let rec aux v ys =
      match ys with
      | [] -> [v]
      | z::zs as l ->
        if v < z
        then v :: l
        else z :: (aux v zs)
    in match xs with
    | [] -> []
    | [x] -> [x]
    | v::vs -> aux v (insertion vs)

  let rec quick_sort: 'a list -> 'a list = function
    | [] -> []
    | x::xs -> let smaller, larger = List.partition (fun y -> y < x) xs
      in let x = (quick_sort smaller)
      and y = (x::quick_sort larger)
      in x @ y
end
