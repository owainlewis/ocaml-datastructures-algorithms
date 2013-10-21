(* General utils *)

module type U = sig
  val range : int -> int -> int list
end

module Utils : U =
struct

  let rec sum_recur = function
    |  [] -> 0
    | x::xs -> x + sum_recur xs

  (* no deferred operations on any recursive call *)
  let sum lst =
    let rec aux' s a =
      match s with
        [] -> a
      | x::xs -> aux' xs (a+x) in
    aux' lst 0

  let range x y =
    let rec aux l item =
      let increment n = n+1
      in
        if (item < y) then aux (l @ [item]) (increment item)
                      else l
    in aux [] x

  (* An int array of random size in range r *)
  let random_array size r = Array.init size (fun _ -> Random.int r)
end

(* Useful operators *)

let (|>) x f = f x 

(* Haskells compose . operator *)
let (>>) f g x = g (f x)

let flat_map f = List.concat >> List.map f

