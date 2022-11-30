(* Merge two sorted lists, l1 and l2, and return the result as a sorted list *)

let rec merge_list l1 l2 =
  match l1 with 
  | [] -> l2
  | (a::b) -> (match l2 with
      | [] -> l1
      | (h::t) when a <= h -> a::(merge_list l2 b)
      | (h::t) -> h::(merge_list l1 t))
      