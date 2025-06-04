open OUnit
open Ods

let test_avl_tree = "AVL Tree" >:::
[
  "insert and member" >:: (fun () ->
    let t = Avl_tree.build [5; 3; 8; 1; 4; 7; 10] in
    assert_bool "Should contain 5" (Avl_tree.member 5 t);
    assert_bool "Should contain 1" (Avl_tree.member 1 t);
    assert_bool "Should contain 10" (Avl_tree.member 10 t);
    assert_bool "Should not contain 6" (not (Avl_tree.member 6 t));
  );
  
  "delete" >:: (fun () ->
    let t = Avl_tree.build [5; 3; 8; 1; 4; 7; 10] in
    let t' = Avl_tree.delete 5 t in
    assert_bool "Should not contain 5" (not (Avl_tree.member 5 t'));
    assert_bool "Should still contain 8" (Avl_tree.member 8 t');
  );
  
  "inorder traversal" >:: (fun () ->
    let t = Avl_tree.build [5; 3; 8; 1; 4; 7; 10] in
    assert_equal (Avl_tree.inorder t) [1; 3; 4; 5; 7; 8; 10]);
]

let test_trie = "Trie" >:::
[
  "insert and search" >:: (fun () ->
    let t = Trie.empty in
    let t = Trie.insert "hello" t in
    let t = Trie.insert "world" t in
    let t = Trie.insert "hi" t in
    assert_bool "Should contain hello" (Trie.search "hello" t);
    assert_bool "Should contain world" (Trie.search "world" t);
    assert_bool "Should contain hi" (Trie.search "hi" t);
    assert_bool "Should not contain bye" (not (Trie.search "bye" t));
  );
  
  "starts_with" >:: (fun () ->
    let t = Trie.build_from_list ["hello"; "hell"; "hemisphere"; "world"] in
    assert_bool "Should find prefix he" (Trie.starts_with "he" t);
    assert_bool "Should find prefix hell" (Trie.starts_with "hell" t);
    assert_bool "Should not find prefix bye" (not (Trie.starts_with "bye" t));
  );
  
  "get_all_words" >:: (fun () ->
    let t = Trie.build_from_list ["cat"; "car"; "cart"] in
    let words = Trie.get_all_words t in
    assert_equal (List.length words) 3;
    assert_bool "Should contain cat" (List.mem "cat" words);
    assert_bool "Should contain car" (List.mem "car" words);
    assert_bool "Should contain cart" (List.mem "cart" words);
  );
]

let test_red_black_tree = "Red Black Tree" >:::
[
  "insert and member" >:: (fun () ->
    let t = Red_black_tree.insert 5 Red_black_tree.Leaf in
    let t = Red_black_tree.insert 3 t in
    let t = Red_black_tree.insert 8 t in
    assert_bool "Should contain 5" (Red_black_tree.member 5 t);
    assert_bool "Should contain 3" (Red_black_tree.member 3 t);
    assert_bool "Should contain 8" (Red_black_tree.member 8 t);
    assert_bool "Should not contain 6" (not (Red_black_tree.member 6 t));
  );
]

let test_splay_tree = "Splay Tree" >:::
[
  "insert and member" >:: (fun () ->
    let t = Splay_tree.build [5; 3; 8; 1; 4; 7; 10] in
    assert_bool "Should contain 5" (Splay_tree.member 5 t);
    assert_bool "Should contain 1" (Splay_tree.member 1 t);
    assert_bool "Should contain 10" (Splay_tree.member 10 t);
    assert_bool "Should not contain 6" (not (Splay_tree.member 6 t));
  );
  
  "delete" >:: (fun () ->
    let t = Splay_tree.build [5; 3; 8; 1; 4; 7; 10] in
    let t' = Splay_tree.delete 5 t in
    assert_bool "Should not contain 5" (not (Splay_tree.member 5 t'));
    assert_bool "Should still contain 8" (Splay_tree.member 8 t'));
  
  "inorder traversal" >:: (fun () ->
    let t = Splay_tree.build [5; 3; 8; 1; 4; 7; 10] in
    assert_equal (Splay_tree.inorder t) [1; 3; 4; 5; 7; 8; 10]);
]

let test_treap = "Treap" >:::
[
  "insert and member" >:: (fun () ->
    let t = Treap.build [5; 3; 8; 1; 4; 7; 10] in
    assert_bool "Should contain 5" (Treap.member 5 t);
    assert_bool "Should contain 1" (Treap.member 1 t);
    assert_bool "Should contain 10" (Treap.member 10 t);
    assert_bool "Should not contain 6" (not (Treap.member 6 t));
  );
  
  "delete" >:: (fun () ->
    let t = Treap.build [5; 3; 8; 1; 4; 7; 10] in
    let t' = Treap.delete 5 t in
    assert_bool "Should not contain 5" (not (Treap.member 5 t'));
    assert_bool "Should still contain 8" (Treap.member 8 t'));
  
  "inorder traversal" >:: (fun () ->
    let t = Treap.build [5; 3; 8; 1; 4; 7; 10] in
    assert_equal (Treap.inorder t) [1; 3; 4; 5; 7; 8; 10]);
]

let test_segment_tree = "Segment Tree" >:::
[
  "range sum" >:: (fun () ->
    let arr = [|1; 3; 5; 7; 9; 11|] in
    let seg_tree = Segment_tree.sum_segment_tree arr in
    assert_equal (Segment_tree.query seg_tree 0 2) 9;
    assert_equal (Segment_tree.query seg_tree 1 4) 24;
    assert_equal (Segment_tree.query seg_tree 0 5) 36;
  );
  
  "update" >:: (fun () ->
    let arr = [|1; 3; 5; 7; 9; 11|] in
    let seg_tree = Segment_tree.sum_segment_tree arr in
    let _ = Segment_tree.update seg_tree 2 10 in
    assert_equal (Segment_tree.query seg_tree 0 2) 14;
    assert_equal (Segment_tree.query seg_tree 1 4) 29;
  );
  
  "min query" >:: (fun () ->
    let arr = [|5; 2; 8; 1; 9; 3|] in
    let seg_tree = Segment_tree.min_segment_tree arr in
    assert_equal (Segment_tree.query seg_tree 0 2) 2;
    assert_equal (Segment_tree.query seg_tree 2 5) 1;
  );
]

let test_fenwick_tree = "Fenwick Tree" >:::
[
  "prefix sum" >:: (fun () ->
    let ft = Fenwick_tree.of_list [3; 2; -1; 6; 5; 4] in
    assert_equal (Fenwick_tree.prefix_sum ft 0) 3;
    assert_equal (Fenwick_tree.prefix_sum ft 2) 4;
    assert_equal (Fenwick_tree.prefix_sum ft 5) 19;
  );
  
  "range sum" >:: (fun () ->
    let ft = Fenwick_tree.of_list [3; 2; -1; 6; 5; 4] in
    assert_equal (Fenwick_tree.range_sum ft 1 3) 7;
    assert_equal (Fenwick_tree.range_sum ft 2 5) 14;
  );
  
  "update" >:: (fun () ->
    let ft = Fenwick_tree.of_list [3; 2; -1; 6; 5; 4] in
    Fenwick_tree.update ft 2 5;
    assert_equal (Fenwick_tree.prefix_sum ft 2) 9;
    assert_equal (Fenwick_tree.range_sum ft 1 3) 12;
  );
]

let test_union_find = "Union Find" >:::
[
  "connected components" >:: (fun () ->
    let edges = [(0, 1); (1, 2); (3, 4); (5, 6); (6, 7); (8, 9)] in
    let num_components = Union_find.connected_components edges 10 in
    assert_equal num_components 4;
  );
  
  "union and find" >:: (fun () ->
    let uf = Union_find.create 10 in
    ignore (Union_find.union uf 0 1);
    ignore (Union_find.union uf 1 2);
    ignore (Union_find.union uf 3 4);
    assert_bool "0 and 2 should be connected" (Union_find.connected uf 0 2);
    assert_bool "3 and 4 should be connected" (Union_find.connected uf 3 4);
    assert_bool "0 and 3 should not be connected" (not (Union_find.connected uf 0 3));
  );
]

let _ = run_test_tt_main (
  "Tree data structures" >:::
  [
    test_avl_tree;
    test_trie;
    test_red_black_tree;
    test_splay_tree;
    test_treap;
    test_segment_tree;
    test_fenwick_tree;
    test_union_find;
  ]
)