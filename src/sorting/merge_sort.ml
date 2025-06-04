(* Merge Sort Implementation *)

(* Merge two sorted lists into a single sorted list *)
let rec merge left right =
  match left, right with
  | [], right -> right
  | left, [] -> left
  | x::xs, y::ys ->
      if x <= y then
        x :: merge xs right
      else
        y :: merge left ys

(* Split a list into two roughly equal parts *)
let split list =
  let rec aux left right = function
    | [] -> (List.rev left, right)
    | [x] -> (List.rev (x::left), right)
    | x::y::rest -> aux (x::left) (y::right) rest
  in
  aux [] [] list

(* Merge sort on lists *)
let rec sort = function
  | [] -> []
  | [x] -> [x]
  | xs ->
      let (left, right) = split xs in
      merge (sort left) (sort right)

(* Merge sort on arrays (in-place) *)
let sort_array arr =
  let len = Array.length arr in
  let aux = Array.make len arr.(0) in
  
  let rec merge_arrays src dest start mid ending =
    let i = ref start in
    let j = ref mid in
    let k = ref start in
    
    while !k < ending do
      if !i >= mid then (
        dest.(!k) <- src.(!j);
        incr j
      ) else if !j >= ending then (
        dest.(!k) <- src.(!i);
        incr i
      ) else if src.(!i) <= src.(!j) then (
        dest.(!k) <- src.(!i);
        incr i
      ) else (
        dest.(!k) <- src.(!j);
        incr j
      );
      incr k
    done
  in
  
  let rec split_merge src dest start ending =
    if ending - start <= 1 then ()
    else
      let mid = start + ((ending - start) / 2) in
      split_merge dest src start mid;
      split_merge dest src mid ending;
      merge_arrays src dest start mid ending
  in
  
  if len <= 1 then arr
  else (
    Array.blit arr 0 aux 0 len;
    split_merge aux arr 0 len;
    arr
  )