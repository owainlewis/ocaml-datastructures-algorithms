(* OCaml distance metrics *)

class point_type x_pos y_pos = object
  val mutable x: float = x_pos
  val mutable y: float = y_pos
  method x = x
  method y = y
end

module Distance = struct
  type point = Point of float * float

  let as_tuple p = match p with Point(x,y) -> (x,y)
  (* This is also known as chessboard distance *)
  let chebyshev p1 p2 =
    let (x1, y1) = as_tuple p1
    and (x2, y2) = as_tuple p2 in
    let a = abs_float (x1 -. x2)
    and b = abs_float (y1 -. y2) in
    max a b

  let euclidean () = ()

  let manhattan () = ()
end

let test x1 y1 x2 y2 =
  let p1 = Distance.Point(x1,y1)
  and p2 = Distance.Point(x2,y2) in
  Distance.chebyshev p1 p2
