(* Bubble Sort Implementation *)

(* Bubble sort for lists *)
let rec sort = function
  | [] -> []
  | lst ->
      let rec bubble = function
        | [] -> []
        | [x] -> [x]
        | x::y::rest ->
            if x > y then
              y :: bubble (x :: rest)
            else
              x :: bubble (y :: rest)
      in
      let pass = bubble lst in
      if pass = lst then
        pass
      else
        sort pass

(* Bubble sort for arrays (in-place) *)
let sort_array arr =
  let n = Array.length arr in
  let sorted = ref false in
  let i = ref 0 in

  while not !sorted && !i < n - 1 do
    sorted := true;
    for j = 0 to n - !i - 2 do
      if arr.(j) > arr.(j + 1) then (
        let temp = arr.(j) in
        arr.(j) <- arr.(j + 1);
        arr.(j + 1) <- temp;
        sorted := false
      )
    done;
    incr i
  done;

  arr