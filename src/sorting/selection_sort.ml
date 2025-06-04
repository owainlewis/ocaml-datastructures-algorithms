(* Selection Sort Implementation *)

(* Selection sort for lists *)
let rec sort = function
  | [] -> []
  | lst ->
      let rec select_min min rest = function
        | [] -> min :: sort rest
        | x::xs ->
            if x < min then
              select_min x (min::rest) xs
            else
              select_min min (x::rest) xs
      in
      match lst with
      | [] -> []
      | x::xs -> select_min x [] xs

(* Selection sort for arrays (in-place) *)
let sort_array arr =
  let n = Array.length arr in
  
  for i = 0 to n - 2 do
    let min_idx = ref i in
    
    for j = i + 1 to n - 1 do
      if arr.(j) < arr.(!min_idx) then
        min_idx := j
    done;
    
    if !min_idx <> i then (
      let temp = arr.(i) in
      arr.(i) <- arr.(!min_idx);
      arr.(!min_idx) <- temp
    )
  done;
  
  arr