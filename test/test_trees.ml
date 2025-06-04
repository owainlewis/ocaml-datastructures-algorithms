open OUnit
open Ods

let test_avl_tree = "AVL Tree" >:::
[
  "insert and member" >:: (fun () ->
    let t = Trees.AVL.build [5; 3; 8; 1; 4; 7; 10] in
    assert_bool "Should contain 5" (Trees.AVL.member 5 t);
    assert_bool "Should contain 1" (Trees.AVL.member 1 t);
    assert_bool "Should contain 10" (Trees.AVL.member 10 t);
    assert_bool "Should not contain 6" (not (Trees.AVL.member 6 t));
  );
  
  "delete" >:: (fun () ->
    let t = Trees.AVL.build [5; 3; 8; 1; 4; 7; 10] in
    let t' = Trees.AVL.delete 5 t in
    assert_bool "Should not contain 5" (not (Trees.AVL.member 5 t'));
    assert_bool "Should still contain 8" (Trees.AVL.member 8 t');
  );
  
  "inorder traversal" >:: (fun () ->
    let t = Trees.AVL.build [5; 3; 8; 1; 4; 7; 10] in
    assert_equal (Trees.AVL.inorder t) [1; 3; 4; 5; 7; 8; 10]);
]

let test_trie = "Trie" >:::
[
  "insert and search" >:: (fun () ->
    let t = Trees.Trie.empty in
    let t = Trees.Trie.insert "hello" t in
    let t = Trees.Trie.insert "world" t in
    let t = Trees.Trie.insert "hi" t in
    assert_bool "Should contain hello" (Trees.Trie.search "hello" t);
    assert_bool "Should contain world" (Trees.Trie.search "world" t);
    assert_bool "Should contain hi" (Trees.Trie.search "hi" t);
    assert_bool "Should not contain bye" (not (Trees.Trie.search "bye" t));
  );
  
  "starts_with" >:: (fun () ->
    let t = Trees.Trie.build_from_list ["hello"; "hell"; "hemisphere"; "world"] in
    assert_bool "Should find prefix he" (Trees.Trie.starts_with "he" t);
    assert_bool "Should find prefix hell" (Trees.Trie.starts_with "hell" t);
    assert_bool "Should not find prefix bye" (not (Trees.Trie.starts_with "bye" t));
  );
  
  "get_all_words" >:: (fun () ->
    let t = Trees.Trie.build_from_list ["cat"; "car"; "cart"] in
    let words = Trees.Trie.get_all_words t in
    assert_equal (List.length words) 3;
    assert_bool "Should contain cat" (List.mem "cat" words);
    assert_bool "Should contain car" (List.mem "car" words);
    assert_bool "Should contain cart" (List.mem "cart" words);
  );
]

let test_red_black_tree = "Red Black Tree" >:::
[
  "insert and member" >:: (fun () ->
    let t = Trees.RedBlack.insert 5 Trees.RedBlack.Leaf in
    let t = Trees.RedBlack.insert 3 t in
    let t = Trees.RedBlack.insert 8 t in
    assert_bool "Should contain 5" (Trees.RedBlack.member 5 t);
    assert_bool "Should contain 3" (Trees.RedBlack.member 3 t);
    assert_bool "Should contain 8" (Trees.RedBlack.member 8 t);
    assert_bool "Should not contain 6" (not (Trees.RedBlack.member 6 t));
  );
]

let test_splay_tree = "Splay Tree" >:::
[
  "insert and member" >:: (fun () ->
    let t = Trees.Splay.build [5; 3; 8; 1; 4; 7; 10] in
    assert_bool "Should contain 5" (Trees.Splay.member 5 t);
    assert_bool "Should contain 1" (Trees.Splay.member 1 t);
    assert_bool "Should contain 10" (Trees.Splay.member 10 t);
    assert_bool "Should not contain 6" (not (Trees.Splay.member 6 t));
  );
  
  "delete" >:: (fun () ->
    let t = Trees.Splay.build [5; 3; 8; 1; 4; 7; 10] in
    let t' = Trees.Splay.delete 5 t in
    assert_bool "Should not contain 5" (not (Trees.Splay.member 5 t'));
    assert_bool "Should still contain 8" (Trees.Splay.member 8 t'));
  
  "inorder traversal" >:: (fun () ->
    let t = Trees.Splay.build [5; 3; 8; 1; 4; 7; 10] in
    assert_equal (Trees.Splay.inorder t) [1; 3; 4; 5; 7; 8; 10]);
]

let test_treap = "Treap" >:::
[
  "insert and member" >:: (fun () ->
    let t = Trees.Treap.build [5; 3; 8; 1; 4; 7; 10] in
    assert_bool "Should contain 5" (Trees.Treap.member 5 t);
    assert_bool "Should contain 1" (Trees.Treap.member 1 t);
    assert_bool "Should contain 10" (Trees.Treap.member 10 t);
    assert_bool "Should not contain 6" (not (Trees.Treap.member 6 t));
  );
  
  "delete" >:: (fun () ->
    let t = Trees.Treap.build [5; 3; 8; 1; 4; 7; 10] in
    let t' = Trees.Treap.delete 5 t in
    assert_bool "Should not contain 5" (not (Trees.Treap.member 5 t'));
    assert_bool "Should still contain 8" (Trees.Treap.member 8 t'));
  
  "inorder traversal" >:: (fun () ->
    let t = Trees.Treap.build [5; 3; 8; 1; 4; 7; 10] in
    assert_equal (Trees.Treap.inorder t) [1; 3; 4; 5; 7; 8; 10]);
]

let test_segment_tree = "Segment Tree" >:::
[
  "range sum" >:: (fun () ->
    let arr = [|1; 3; 5; 7; 9; 11|] in
    let seg_tree = Trees.SegmentTree.sum_segment_tree arr in
    assert_equal (Trees.SegmentTree.query seg_tree 0 2) 9;
    assert_equal (Trees.SegmentTree.query seg_tree 1 4) 24;
    assert_equal (Trees.SegmentTree.query seg_tree 0 5) 36;
  );
  
  "update" >:: (fun () ->
    let arr = [|1; 3; 5; 7; 9; 11|] in
    let seg_tree = Trees.SegmentTree.sum_segment_tree arr in
    let _ = Trees.SegmentTree.update seg_tree 2 10 in
    assert_equal (Trees.SegmentTree.query seg_tree 0 2) 14;
    assert_equal (Trees.SegmentTree.query seg_tree 1 4) 29;
  );
  
  "min query" >:: (fun () ->
    let arr = [|5; 2; 8; 1; 9; 3|] in
    let seg_tree = Trees.SegmentTree.min_segment_tree arr in
    assert_equal (Trees.SegmentTree.query seg_tree 0 2) 2;
    assert_equal (Trees.SegmentTree.query seg_tree 2 5) 1;
  );
]

let test_fenwick_tree = "Fenwick Tree" >:::
[
  "prefix sum" >:: (fun () ->
    let ft = Trees.FenwickTree.of_list [3; 2; -1; 6; 5; 4] in
    assert_equal (Trees.FenwickTree.prefix_sum ft 0) 3;
    assert_equal (Trees.FenwickTree.prefix_sum ft 2) 4;
    assert_equal (Trees.FenwickTree.prefix_sum ft 5) 19;
  );
  
  "range sum" >:: (fun () ->
    let ft = Trees.FenwickTree.of_list [3; 2; -1; 6; 5; 4] in
    assert_equal (Trees.FenwickTree.range_sum ft 1 3) 7;
    assert_equal (Trees.FenwickTree.range_sum ft 2 5) 14;
  );
  
  "update" >:: (fun () ->
    let ft = Trees.FenwickTree.of_list [3; 2; -1; 6; 5; 4] in
    Trees.FenwickTree.update ft 2 5;
    assert_equal (Trees.FenwickTree.prefix_sum ft 2) 9;
    assert_equal (Trees.FenwickTree.range_sum ft 1 3) 12;
  );
]

let test_union_find = "Union Find" >:::
[
  "connected components" >:: (fun () ->
    let edges = [(0, 1); (1, 2); (3, 4); (5, 6); (6, 7); (8, 9)] in
    let num_components = Trees.UnionFind.connected_components edges 10 in
    assert_equal num_components 4;
  );
  
  "union and find" >:: (fun () ->
    let uf = Trees.UnionFind.create 10 in
    ignore (Trees.UnionFind.union uf 0 1);
    ignore (Trees.UnionFind.union uf 1 2);
    ignore (Trees.UnionFind.union uf 3 4);
    assert_bool "0 and 2 should be connected" (Trees.UnionFind.connected uf 0 2);
    assert_bool "3 and 4 should be connected" (Trees.UnionFind.connected uf 3 4);
    assert_bool "0 and 3 should not be connected" (not (Trees.UnionFind.connected uf 0 3));
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