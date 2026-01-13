(* Shuffle Algorithms *)

(* Fisher-Yates (Knuth) shuffle - O(n) in-place array shuffle *)
let fisher_yates arr =
  let n = Array.length arr in
  for i = n - 1 downto 1 do
    let j = Random.int (i + 1) in
    let temp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- temp
  done;
  arr

(* Shuffle a list by converting to array *)
let shuffle_list lst =
  let arr = Array.of_list lst in
  let _ = fisher_yates arr in
  Array.to_list arr

(* Generate a random permutation of 0..n-1 *)
let random_permutation n =
  let arr = Array.init n Fun.id in
  fisher_yates arr

(* Randomly sample k elements from an array without replacement *)
let sample k arr =
  let n = Array.length arr in
  if k > n then invalid_arg "k cannot exceed array length";
  let copy = Array.copy arr in
  for i = 0 to k - 1 do
    let j = i + Random.int (n - i) in
    let temp = copy.(i) in
    copy.(i) <- copy.(j);
    copy.(j) <- temp
  done;
  Array.sub copy 0 k

(* Randomly sample k elements from a list *)
let sample_list k lst =
  Array.to_list (sample k (Array.of_list lst))
