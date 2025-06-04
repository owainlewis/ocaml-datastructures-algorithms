(* Insertion Sort Implementation *)

(* Insertion sort for lists *)
let rec sort = function
  | [] -> []
  | x::xs -> insert x (sort xs)

and insert x = function
  | [] -> [x]
  | y::ys as l ->
      if x <= y then x :: l
      else y :: insert x ys

(* Insertion sort for arrays (in-place) *)
let sort_array arr =
  let n = Array.length arr in
  
  for i = 1 to n - 1 do
    let key = arr.(i) in
    let j = ref (i - 1) in
    
    while !j >= 0 && arr.(!j) > key do
      arr.(!j + 1) <- arr.(!j);
      decr j
    done;
    
    arr.(!j + 1) <- key
  done;
  
  arr