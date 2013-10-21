(* Sorting algorithms functional and imp *)

open Utils

let rec bubble_sort l =
  let rec aux = function
    | [] -> []
    | x::y::xs -> 
       if x > y then y::aux(x::xs)
                else x::aux(y::xs)
    | x::xs -> x :: aux xs
  in let p = aux l in
    if l <> p then bubble_sort p
              else l

