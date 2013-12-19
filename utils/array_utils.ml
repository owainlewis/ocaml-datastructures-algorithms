(* Array utils *)

module ArrayUtil = struct

  let swap i j arr =
    let temp = xs.(j) in
    xs.(j) <- xs.(i)
    xs.(i) <- temp
end
