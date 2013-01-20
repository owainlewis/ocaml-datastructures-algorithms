(* General utils *)

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

