(* Random array shuffle *)

let random_shuffle arr =
    let swap i j a =
      let temp = xs.(j) in
      xs.(j) <- xs.(i)
      xs.(i) <- temp
    in arr
