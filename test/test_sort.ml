open OUnit2
open Ods

let test_fixture = "Sorting algorithms" >:::
[
  "bubble" >:: (fun _ ->
      assert_equal (Sorting.Bubble.sort [1;3;2;4;5;0]) [0;1;2;3;4;5]);
  
  "insertion" >:: (fun _ ->
      assert_equal (Sorting.Insertion.sort [5;2;4;6;1;3]) [1;2;3;4;5;6]);
  
  "selection" >:: (fun _ ->
      assert_equal (Sorting.Selection.sort [5;2;4;6;1;3]) [1;2;3;4;5;6]);
  
  "merge" >:: (fun _ ->
      assert_equal (Sorting.Merge.sort [5;2;4;6;1;3]) [1;2;3;4;5;6]);
  
  "quick" >:: (fun _ ->
      assert_equal (Sorting.Quick.sort [5;2;4;6;1;3]) [1;2;3;4;5;6]);
  
  "heap" >:: (fun _ ->
      assert_equal (Sorting.Heap.sort [5;2;4;6;1;3]) [1;2;3;4;5;6]);

  "counting" >:: (fun _ ->
      assert_equal (Sorting.Counting.sort [5;2;4;6;1;3]) [1;2;3;4;5;6]);

  "radix" >:: (fun _ ->
      assert_equal (Sorting.Radix.sort [5;2;4;6;1;3]) [1;2;3;4;5;6]);
  
  "smart_sort" >:: (fun _ ->
      assert_equal (Sorting.sort_list [5;2;4;6;1;3]) [1;2;3;4;5;6]);
]

let _ = run_test_tt_main test_fixture
