(* Heap Sort Implementation *)

(* Heapify a subtree rooted at index i, maintaining max-heap property *)
let heapify arr size i =
  let largest = ref i in
  let left = 2 * i + 1 in
  let right = 2 * i + 2 in
  
  (* If left child is larger than root *)
  if left < size && arr.(left) > arr.(!largest) then
    largest := left;
  
  (* If right child is larger than largest so far *)
  if right < size && arr.(right) > arr.(!largest) then
    largest := right;
  
  (* If largest is not root *)
  if !largest <> i then (
    let temp = arr.(i) in
    arr.(i) <- arr.(!largest);
    arr.(!largest) <- temp;
    
    (* Recursively heapify the affected sub-tree *)
    heapify arr size !largest
  )

(* Build a max heap *)
let build_heap arr =
  let n = Array.length arr in
  
  (* Start from the last non-leaf node and heapify all nodes *)
  for i = (n / 2) - 1 downto 0 do
    heapify arr n i
  done

(* Heap sort implementation for arrays (in-place) *)
let sort arr =
  let n = Array.length arr in
  
  (* Build max heap *)
  build_heap arr;
  
  (* Extract elements one by one from heap *)
  for i = n - 1 downto 1 do
    (* Move current root to end *)
    let temp = arr.(0) in
    arr.(0) <- arr.(i);
    arr.(i) <- temp;
    
    (* Heapify the reduced heap *)
    heapify arr i 0
  done;
  
  arr

(* Heap sort implementation for lists *)
let sort_list lst =
  let arr = Array.of_list lst in
  let sorted = sort arr in
  Array.to_list sorted