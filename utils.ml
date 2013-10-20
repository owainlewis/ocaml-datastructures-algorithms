(* General utils *)

module type U = sig
  val range : int -> int -> int list
  val inc   : int -> int
  val dec   : int -> int
end

module Utils : U =
struct

let range x y =
  let rec aux l item =
    let increment n = n + 1
    in
      if (item < y) then aux (l @ [item]) (increment item)
                    else l
  in aux [] x

  let inc x = x + 1
  let dec x = x - 1

  (* An int array of random size in range r *)
  let random_array size r =
    Array.init size (fun _ -> Random.int r)
end

(* Useful operators *)

let (|>) x f = f x 

(* Haskells compose . operator *)
let (>>) f g x = g (f x)

let flat_map f = List.concat >> List.map f

