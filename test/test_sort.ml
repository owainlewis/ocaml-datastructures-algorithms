open OUnit
open Ods

let test_fixture = "Sorting algorithms" >:::
[
  "bubble" >:: (fun () ->
      assert_equal (Sorting.Bubble.sort [1;3;2;4;5;0]) [0;1;2;3;4;5]);
  
  "insertion" >:: (fun () ->
      assert_equal (Sorting.Insertion.sort [5;2;4;6;1;3]) [1;2;3;4;5;6]);
  
  "selection" >:: (fun () ->
      assert_equal (Sorting.Selection.sort [5;2;4;6;1;3]) [1;2;3;4;5;6]);
  
  "merge" >:: (fun () ->
      assert_equal (Sorting.Merge.sort [5;2;4;6;1;3]) [1;2;3;4;5;6]);
  
  "quick" >:: (fun () ->
      assert_equal (Sorting.Quick.sort [5;2;4;6;1;3]) [1;2;3;4;5;6]);
  
  "heap" >:: (fun () ->
      assert_equal (Sorting.Heap.sort_list [5;2;4;6;1;3]) [1;2;3;4;5;6]);
  
  "counting" >:: (fun () ->
      assert_equal (Sorting.Counting.sort_list [5;2;4;6;1;3]) [1;2;3;4;5;6]);
  
  "radix" >:: (fun () ->
      assert_equal (Sorting.Radix.sort_list [5;2;4;6;1;3]) [1;2;3;4;5;6]);
  
  "smart_sort" >:: (fun () ->
      assert_equal (Sorting.sort_list [5;2;4;6;1;3]) [1;2;3;4;5;6]);
]

let _ = run_test_tt test_fixture
