(* Counting sort *)

let arr = [|2;1;4;5;1|]

let arrayf f arr =
  if Array.length arr <> 0 then 
    let result = Array.fold_left (fun r v -> f r v) arr.(0) arr in Some(result)
  else None

let array_min arr = arrayf min arr
let array_max arr = arrayf max arr
let unsafe_aget = function | Some(x) -> x | None -> failwith "Empty array"

let counting_sort arr =
  let hi  = unsafe_aget (array_min arr) and
      low = unsafe_aget (array_max arr) in
  let count = Array.make (hi-(low+1)) 0 in
  Array.iter (fun i -> count.(i-low) <- count.(i-low) + 1) arr;
  let result = (Array.to_list (Array.mapi (fun i x -> Array.make x (low+i)) count))
  in Array.concat result
