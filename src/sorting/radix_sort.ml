(* Radix Sort Implementation *)

(* Helper function to get the digit at a specific position using integer math *)
let get_digit num position =
  let rec pow10 n acc = if n <= 0 then acc else pow10 (n - 1) (acc * 10) in
  (num / pow10 position 1) mod 10

(* Count sort implementation for radix sort *)
let count_sort arr position =
  let n = Array.length arr in
  let output = Array.make n 0 in
  let count = Array.make 10 0 in

  (* Count occurrences of each digit *)
  for i = 0 to n - 1 do
    let digit = get_digit arr.(i) position in
    count.(digit) <- count.(digit) + 1
  done;

  (* Update count array to contain positions *)
  for i = 1 to 9 do
    count.(i) <- count.(i) + count.(i - 1)
  done;

  (* Build output array *)
  for i = n - 1 downto 0 do
    let digit = get_digit arr.(i) position in
    output.(count.(digit) - 1) <- arr.(i);
    count.(digit) <- count.(digit) - 1
  done;

  (* Copy output to original array *)
  Array.blit output 0 arr 0 n

(* Radix sort for non-negative integers *)
let sort_non_negative arr =
  if Array.length arr = 0 then arr
  else
    (* Find the maximum number to know number of digits *)
    let max_val = Array.fold_left max 0 arr in

    (* Apply counting sort for each digit position *)
    let rec sort_digits position divisor =
      if max_val / divisor > 0 then (
        count_sort arr position;
        sort_digits (position + 1) (divisor * 10)
      )
    in

    sort_digits 0 1;
    arr

(* Radix sort for any integers (handles negative values) *)
let sort_array arr =
  if Array.length arr = 0 then arr
  else
    (* Split into negative and non-negative arrays *)
    let neg, non_neg = Array.to_list arr |> List.partition (fun x -> x < 0) in

    (* Sort absolute values of negative numbers *)
    let neg_abs = List.map abs neg |> Array.of_list in
    let _ = sort_non_negative neg_abs in

    (* Reverse and negate to get sorted negative numbers *)
    let sorted_neg = Array.to_list neg_abs
                    |> List.rev
                    |> List.map (fun x -> -x) in

    (* Sort non-negative numbers *)
    let non_neg_arr = Array.of_list non_neg in
    let _ = sort_non_negative non_neg_arr in

    (* Combine the two sorted arrays *)
    sorted_neg @ (Array.to_list non_neg_arr)
    |> Array.of_list

(* Radix sort for list of integers *)
let sort lst =
  let arr = Array.of_list lst in
  let sorted = sort_array arr in
  Array.to_list sorted
