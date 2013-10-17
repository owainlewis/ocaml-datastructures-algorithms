(* General utils *)

module Util
  struct
  end

let range (x: int) (y: int) =
  let rec aux l item =
    let increment n = n + 1
    in
      match (item < y) with
      | true  -> aux (l @ [item]) (increment item)
      | false -> l
  in aux [] x

let rand_int (limit: int) : int =
  Random.int limit

(* An int array of random size in range r *)
let random_array size r =
  Array.init size (fun _ -> Random.int r)

(* Useful operators *)

let (|>) x f = f x 

(* Haskells compose . operator *)
let (>>) f g x = g (f x)

let inc x = x + 1
let dec x = x - 1

let flat_map f = List.concat >> List.map f

