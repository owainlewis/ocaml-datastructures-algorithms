(* Counting Sort Implementation *)

(* Counting sort for array of non-negative integers *)
let sort_non_negative arr =
  if Array.length arr = 0 then arr
  else
    (* Find maximum element to determine count array size *)
    let max_val = Array.fold_left max 0 arr in
    
    (* Create a count array to store count of each value *)
    let count = Array.make (max_val + 1) 0 in
    
    (* Count occurrences of each element *)
    Array.iter (fun x -> count.(x) <- count.(x) + 1) arr;
    
    (* Update count array to contain actual positions *)
    for i = 1 to max_val do
      count.(i) <- count.(i) + count.(i - 1)
    done;
    
    (* Build output array *)
    let output = Array.make (Array.length arr) 0 in
    for i = Array.length arr - 1 downto 0 do
      output.(count.(arr.(i)) - 1) <- arr.(i);
      count.(arr.(i)) <- count.(arr.(i)) - 1
    done;
    
    (* Copy output array to original array *)
    Array.blit output 0 arr 0 (Array.length arr);
    arr

(* Counting sort for array of integers (can handle negative values) *)
let sort arr =
  if Array.length arr = 0 then arr
  else
    (* Find minimum and maximum elements *)
    let min_val = Array.fold_left min Int.max_int arr in
    let max_val = Array.fold_left max Int.min_int arr in
    
    (* Shift all values to be non-negative *)
    let shifted = Array.map (fun x -> x - min_val) arr in
    
    (* Apply counting sort on shifted array *)
    let _ = sort_non_negative shifted in
    
    (* Shift back to original values *)
    Array.map (fun x -> x + min_val) shifted

(* Counting sort for list of integers *)
let sort_list lst =
  let arr = Array.of_list lst in
  let sorted = sort arr in
  Array.to_list sorted