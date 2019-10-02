open OUnit
open Ods

let test_fixture = "Sorting algorithms" >:::
[
  "bubble" >:: ( fun () ->      
      assert_equal (Sorting.bubble [1;3;2;4;5;0]) [0;1;2;3;4;5]);
]

let _ = run_test_tt test_fixture
