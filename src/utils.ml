(* General utils *)

let range (x : int) (y : int) =
  let rec aux lst acc =
    if acc < y then
      lst :: (acc + 1)
    else lst
  in aux [x] 0
  
let min (a : 'int list) : int = 
  List.fold_left (fun x y -> if x < y then x else y)

let rand_int (limit : int) : int =
  Random.int limit 


