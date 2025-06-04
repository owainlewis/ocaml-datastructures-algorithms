(* Quick Sort Implementation *)

(* Partition a list based on a pivot element *)
let partition pivot = 
  let rec aux left right = function
    | [] -> (left, right)
    | x::xs ->
        if x <= pivot then
          aux (x::left) right xs
        else
          aux left (x::right) xs
  in
  aux [] []

(* Quick sort for lists *)
let rec sort = function
  | [] -> []
  | pivot::rest ->
      let (left, right) = partition pivot rest in
      sort left @ [pivot] @ sort right

(* Quick sort for arrays (in-place) *)
let sort_array arr =
  (* Swap elements in array *)
  let swap i j =
    let temp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- temp
  in
  
  (* Partition function - Lomuto partition scheme *)
  let partition low high =
    let pivot = arr.(high) in
    let i = ref (low - 1) in
    
    for j = low to high - 1 do
      if arr.(j) <= pivot then (
        incr i;
        swap !i j
      )
    done;
    
    let pivot_pos = !i + 1 in
    swap pivot_pos high;
    pivot_pos
  in
  
  (* Quick sort function *)
  let rec quick_sort low high =
    if low < high then
      let pivot_pos = partition low high in
      quick_sort low (pivot_pos - 1);
      quick_sort (pivot_pos + 1) high
  in
  
  let len = Array.length arr in
  if len > 1 then
    quick_sort 0 (len - 1);
  arr

(* Random Pivot Quick sort - better performance on almost sorted data *)
let sort_random_pivot arr =
  let len = Array.length arr in
  
  (* Swap elements in array *)
  let swap i j =
    let temp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- temp
  in
  
  (* Random partition - choose a random pivot for better average performance *)
  let partition low high =
    let rand_idx = low + Random.int (high - low + 1) in
    swap rand_idx high;  (* Move random element to end *)
    
    let pivot = arr.(high) in
    let i = ref (low - 1) in
    
    for j = low to high - 1 do
      if arr.(j) <= pivot then (
        incr i;
        swap !i j
      )
    done;
    
    let pivot_pos = !i + 1 in
    swap pivot_pos high;
    pivot_pos
  in
  
  (* Quick sort function *)
  let rec quick_sort low high =
    if low < high then
      let pivot_pos = partition low high in
      quick_sort low (pivot_pos - 1);
      quick_sort (pivot_pos + 1) high
  in
  
  if len > 1 then (
    Random.self_init();
    quick_sort 0 (len - 1)
  );
  arr