(* Distance Metrics *)

type point = float * float

(* Euclidean distance - straight line distance *)
let euclidean (x1, y1) (x2, y2) =
  let dx = x1 -. x2 in
  let dy = y1 -. y2 in
  sqrt (dx *. dx +. dy *. dy)

(* Manhattan distance - sum of absolute differences (taxi-cab distance) *)
let manhattan (x1, y1) (x2, y2) =
  abs_float (x1 -. x2) +. abs_float (y1 -. y2)

(* Chebyshev distance - maximum of absolute differences (chessboard distance) *)
let chebyshev (x1, y1) (x2, y2) =
  max (abs_float (x1 -. x2)) (abs_float (y1 -. y2))

(* Minkowski distance - generalization of Euclidean and Manhattan *)
let minkowski p (x1, y1) (x2, y2) =
  let dx = abs_float (x1 -. x2) in
  let dy = abs_float (y1 -. y2) in
  (dx ** p +. dy ** p) ** (1. /. p)

(* N-dimensional Euclidean distance *)
let euclidean_nd p1 p2 =
  let sq_diff = List.map2 (fun a b -> (a -. b) ** 2.) p1 p2 in
  sqrt (List.fold_left (+.) 0. sq_diff)

(* N-dimensional Manhattan distance *)
let manhattan_nd p1 p2 =
  let abs_diff = List.map2 (fun a b -> abs_float (a -. b)) p1 p2 in
  List.fold_left (+.) 0. abs_diff

(* Hamming distance - number of positions where elements differ *)
let hamming s1 s2 =
  if String.length s1 <> String.length s2 then
    invalid_arg "Strings must have equal length"
  else
    let count = ref 0 in
    String.iteri (fun i c ->
      if c <> s2.[i] then incr count
    ) s1;
    !count
