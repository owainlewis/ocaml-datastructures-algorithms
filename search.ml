(* Search *)

open Utils

let r = Utils.random_list 100 100

(* Nice easy one to start with *)
let rec linear_search v = function
  | [] -> None
  | x::xs -> if x = v then Some(x) 
                      else linear_search value xs
