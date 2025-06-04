(* Trees Module - Entry point for all tree data structures *)

module AVL = struct
  type 'a tree = 'a Avl_tree.tree =
    | Empty
    | Node of 'a * 'a tree * 'a tree * int
  
  let empty = Avl_tree.Empty
  let insert = Avl_tree.insert
  let delete = Avl_tree.delete
  let member = Avl_tree.member
  let inorder = Avl_tree.inorder
  let preorder = Avl_tree.preorder
  let postorder = Avl_tree.postorder
  let build = Avl_tree.build
end

module BST = struct
  type 'a tree = 'a Binary_search_tree.tree =
    | Leaf
    | Node of 'a tree * 'a * 'a tree
  
  let leaf = Binary_search_tree.leaf
  let node = Binary_search_tree.node
  let insert = Binary_search_tree.insert
  let delete = Binary_search_tree.delete
  let member = Binary_search_tree.member
  let min_value = Binary_search_tree.min_value
  let max_value = Binary_search_tree.max_value
  let height = Binary_search_tree.height
  let build = Binary_search_tree.build
end

module RedBlack = struct
  type color = Red_black_tree.color =
    | Red
    | Black
  
  type 'a tree = 'a Red_black_tree.tree =
    | Leaf
    | Node of color * 'a * 'a tree * 'a tree
  
  let insert = Red_black_tree.insert
  let member = Red_black_tree.member
end

module Splay = struct
  type 'a tree = 'a Splay_tree.tree =
    | Empty
    | Node of 'a tree * 'a * 'a tree
  
  let insert = Splay_tree.insert
  let delete = Splay_tree.delete
  let member = Splay_tree.member
  let find = Splay_tree.find
  let inorder = Splay_tree.inorder
  let build = Splay_tree.build
end

module Treap = struct
  type 'a tree = 'a Treap.tree =
    | Empty
    | Node of 'a * int * 'a tree * 'a tree
  
  let insert = Treap.insert
  let delete = Treap.delete
  let member = Treap.member
  let find_min = Treap.find_min
  let find_max = Treap.find_max
  let inorder = Treap.inorder
  let preorder = Treap.preorder
  let build = Treap.build
end

module Trie = struct
  type trie = Trie.trie
  
  let empty = Trie.empty
  let insert = Trie.insert
  let search = Trie.search
  let starts_with = Trie.starts_with
  let get_all_words = Trie.get_all_words
  let delete = Trie.delete
  let count_words = Trie.count_words
  let build_from_list = Trie.build_from_list
  let autocomplete = Trie.autocomplete
end

module SegmentTree = struct
  type 'a t = 'a Segment_tree.t
  
  let build = Segment_tree.build
  let query = Segment_tree.query
  let update = Segment_tree.update
  let sum_segment_tree = Segment_tree.sum_segment_tree
  let min_segment_tree = Segment_tree.min_segment_tree
  let max_segment_tree = Segment_tree.max_segment_tree
  let gcd_segment_tree = Segment_tree.gcd_segment_tree
  let product_segment_tree = Segment_tree.product_segment_tree
end

module FenwickTree = struct
  type t = Fenwick_tree.t
  
  let create = Fenwick_tree.create
  let create_empty = Fenwick_tree.create_empty
  let prefix_sum = Fenwick_tree.prefix_sum
  let range_sum = Fenwick_tree.range_sum
  let update = Fenwick_tree.update
  let set = Fenwick_tree.set
  let get = Fenwick_tree.get
  let of_list = Fenwick_tree.of_list
  let to_list = Fenwick_tree.to_list
  let find_largest_with_sum_leq = Fenwick_tree.find_largest_with_sum_leq
end

module BTree = struct
  type 'a entry = 'a Btree.entry = {
    key: 'a;
    value: 'a;
  }
  
  type 'a node = 'a Btree.node = {
    entries: 'a entry array;
    mutable n_entries: int;
    mutable children: 'a btree array option;
  }
  
  and 'a btree = 'a Btree.btree = 'a node option
  
  type 'a t = 'a Btree.t = {
    order: int;
    mutable root: 'a btree;
  }
  
  let create = Btree.create
  let search = Btree.search
  let insert = Btree.insert
  let traverse = Btree.traverse
end

module SuffixTree = struct
  type node = Suffix_tree.node
  type edge = Suffix_tree.edge
  type t = Suffix_tree.t
  
  let create = Suffix_tree.create
  let contains = Suffix_tree.contains
  let find_all = Suffix_tree.find_all
  let visualize = Suffix_tree.visualize
end

module UnionFind = struct
  type t = Union_find.t
  
  let create = Union_find.create
  let find = Union_find.find
  let union = Union_find.union
  let connected = Union_find.connected
  let count = Union_find.count
  let get_set = Union_find.get_set
  let get_all_sets = Union_find.get_all_sets
  let has_cycle = Union_find.has_cycle
  let kruskal = Union_find.kruskal
  let connected_components = Union_find.connected_components
end