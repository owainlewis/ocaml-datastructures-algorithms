(** Sorting Algorithms **)

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
