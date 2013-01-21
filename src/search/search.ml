(* Search Algorithms *)

let sample = [|1;2;3;4;5;6;7;8;9|];;

let rec linear_search (a : 'a list) (value: 'a) =
  match a with
    | [] -> false
    | h::t -> if value == h then true else linear_search t value

(** Binary search for array index value *)

let binary_search (a : 'a array) (value : 'a) =
  let rec aux min max = 
    if min = max then
      if a.(min) = value then
        min
      else
        raise Not_found
    else
      (* Divide the search space *)
      let mid_point = (min + max / 2) in
        if a.(mid_point) > value then
          aux min (mid_point - 1)
        else if a.(mid_point) < value then
          aux (mid_point + 1) max
        else mid_point
  in 
  aux 0 (Array.length a)

