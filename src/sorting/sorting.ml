(* Sorting Module - Entry point for all sorting algorithms *)

module Bubble = struct
  let sort = Bubble_sort.sort
  let sort_array = Bubble_sort.sort_array
end

module Insertion = struct
  let sort = Insertion_sort.sort
  let sort_array = Insertion_sort.sort_array
end

module Selection = struct
  let sort = Selection_sort.sort
  let sort_array = Selection_sort.sort_array
end

module Quick = struct
  let sort = Quick_sort.sort
  let sort_array = Quick_sort.sort_array
  let sort_random_pivot = Quick_sort.sort_random_pivot
end

module Merge = struct
  let sort = Merge_sort.sort
  let sort_array = Merge_sort.sort_array
end

module Heap = struct
  let sort = Heap_sort.sort
  let sort_list = Heap_sort.sort_list
end

module Counting = struct
  let sort = Counting_sort.sort
  let sort_list = Counting_sort.sort_list
  let sort_non_negative = Counting_sort.sort_non_negative
end

module Radix = struct
  let sort = Radix_sort.sort
  let sort_list = Radix_sort.sort_list
  let sort_non_negative = Radix_sort.sort_non_negative
end

(* Helper functions *)

(* Sort a list using the most appropriate algorithm based on data characteristics *)
let sort_list lst =
  match lst with
  | [] | [_] -> lst  (* Empty or single element list *)
  | _ ->
      (* For small lists, use insertion sort *)
      if List.length lst < 20 then
        Insertion.sort lst
      (* For larger lists, use merge sort as a general-purpose algorithm *)
      else
        Merge.sort lst

(* Sort an array using the most appropriate algorithm based on data characteristics *)
let sort_array arr =
  let len = Array.length arr in
  match len with
  | 0 | 1 -> arr  (* Empty or single element array *)
  | _ ->
      (* For small arrays, use insertion sort *)
      if len < 20 then
        Insertion.sort_array arr
      (* For medium arrays, use quick sort *)
      else if len < 1000 then
        Quick.sort_random_pivot arr
      (* For larger arrays, use heap sort as it guarantees O(n log n) worst case *)
      else
        Heap.sort arr