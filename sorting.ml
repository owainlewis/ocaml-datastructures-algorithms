(* Sorting algorithms functional and imp *)

open Utils

let unsorted = Utils.random_list 100 100

let rec swap (l, n) =
    let rec loop xs count acc =
      match xs with
      | _ when count = n -> xs @ List.rev acc
      | [] -> List.rev acc
      | h::t -> loop t (count+1) (h::acc)
     in loop l 0 []

(* Mutable state *)
let swapm xs i j =
  let temp = xs.(i) in
  xs.(i) <- xs.(j);
  xs.(j) <- temp
 
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

