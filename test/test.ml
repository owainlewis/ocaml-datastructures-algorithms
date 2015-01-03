(* *********************************************************** *)
(* Test Suite                                                  *)
(* *********************************************************** *)

open OUnit
open Sort

(* *********************************************************** *)
(* Sorts                                                       *)
(* *********************************************************** *)

let test_sorting_algorithm f =
  let unsorted = [9;2;4;3;5;6;7;1;8;0]
  and sorted   = [0;1;2;3;4;5;6;7;8;9] in
  assert_equal (f unsorted) sorted

let test_selection_sort () =
  test_sorting_algorithm Sort.selection

(* *********************************************************** *)
(* Test runner                                                 *)
(* *********************************************************** *)

let suite = "suite">::: ["selection sort">:: test_selection_sort;]

let _ = run_test_tt_main suite
