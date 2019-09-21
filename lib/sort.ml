(** Sorting Algorithms **)

let rec bubble xs =
  let rec switch_all ys = match ys with
    | []    -> []
    | x::[] -> [x]
    | x::y::xs when x > y -> y::switch_all(x::xs)
    | x::y::xs -> x :: switch_all(y::xs)
  in
  let sweep = switch_all xs in
  if sweep = xs then sweep else bubble sweep
