open OUnit

(* Test Fixture *)
let test_fixture = "Binary Search Trees" >:::
[
  "add" >:: ( fun () -> 
    assert_equal 4 1;
    assert_equal 0 0;
  );
]

let _ = run_test_tt test_fixture
