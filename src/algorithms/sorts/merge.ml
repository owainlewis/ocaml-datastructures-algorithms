(* Merge sort *)

module MergeSort = struct

let rec split_at n xs =
  match n, xs with
    0, xs -> [], xs
  | n, x::xs when n > 0 ->
    let y, z = split_at (n-1) xs in
      x::y, z
  | _, _ -> failwith "Negative index"

(* Divide a list into two parts *)
let divide xs = split_at (List.length xs / 2) xs

let rec sort cmp = function
    [] -> []
  | [x] -> [x]
  | xs ->
      let xs, ys = divide xs in
      List.merge cmp (merge_sort cmp xs) (merge_sort cmp ys)

end

let eg = MergeSort.sort compare [8;6;4;2;1;3;5;7;9]
