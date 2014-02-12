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
  let px point = match point with
    | Point(x, _) -> Some(x)
    | _ -> None
  let py point = match point with
    | Point(_, y) -> Some(y)
    | _ -> None
  (* This is also known as chessboard distance *)
  let chebyshev (x1,y1) (x2,y2) =
    let a = abs_float (x1 -. x2)
    and b = abs_float (y1 -. y2) in
    max a b
 (* The euclidean distance for a two points is simply
    euclidean distance = ((x, y), (a, b)) = sqrt (x - a)2 + (y - b)2 *)
  let euclidean (x1,y1) (x2,y2) =
    let v1 = (x1 -. x2)
    and v2 = (y1 -. y2)
    in let sq = (fun x -> x *. x)
       in sqrt ((sq v1) +. (sq v2))

  let manhattan (x1,y1) (x2,y2) = ()
end

let test x1 y1 x2 y2 =
  let p1 = Distance.Point(x1,y1)
  and p2 = Distance.Point(x2,y2) in
  Distance.chebyshev p1 p2
