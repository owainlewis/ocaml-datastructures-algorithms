(* Bubble Sort *)

module type BUBBLE =
  sig
    val bubble_sort : 'a list -> 'a list
  end

module Sorting : BUBBLE = struct
  let switch_head = function
      | [] -> []
      | x::y::xs when x > y -> y::x::xs
      | xs -> xs
  let rec bubble_sort xs =
    let rec switch_all = function
      | []    -> []
      | x::[] -> [x]
      | x::y::xs when x > y -> y::switch_all(x::xs)
      | x::y::xs -> x :: switch_all(y::xs)
    in
    let v = switch_all xs in
    (* if nothing has changed it's sorted otherwise do another sweep *)
    if v = xs then v
	      else bubble_sort v
end

let test () = if Sorting.bubble_sort [4;5;1;3;2] = [1;2;3;4;5]
              then "PASS"
              else "FAIL"
