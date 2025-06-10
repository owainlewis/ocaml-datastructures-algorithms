# OCaml Data Structures and Algorithms

A comprehensive library of data structures and algorithms implemented in OCaml. This library is designed for educational purposes to demonstrate how common data structures and algorithms can be implemented in a functional programming language.

## Data Structures

### Tree-based Data Structures
- Binary Search Tree
- AVL Tree (Self-balancing)
- Red-Black Tree (Self-balancing)
- Splay Tree (Self-adjusting)
- Treap (Randomized BST)
- B-Tree (Multi-way search tree)
- Trie (Prefix tree)
- Segment Tree (Range queries)
- Fenwick Tree / Binary Indexed Tree (Prefix sums)
- Suffix Tree (Pattern matching)
- Union-Find / Disjoint Set (Connectivity)

### Sorting Algorithms
- Bubble Sort
- Selection Sort
- Insertion Sort
- Merge Sort
- Quick Sort
- Heap Sort
- Counting Sort
- Radix Sort

## Building

```bash
# Setup OPAM environment
eval $(opam env)

# Build the project
make build

# Run tests
make test

# Start OCaml REPL with library loaded
make utop
```

## Usage

```ocaml
# Using sorting algorithms
open Ods

(* Using basic sorting *)
let sorted_list = Sorting.Bubble.sort [5; 2; 1; 4; 3];;
let sorted_array = Sorting.Quick.sort_array [|5; 2; 1; 4; 3|];;

(* Using automatic algorithm selection *)
let optimal_sort = Sorting.sort_list [5; 2; 1; 4; 3];;

# Using tree data structures
open Ods

(* Create and use an AVL tree *)
let avl = Trees.AVL.build [5; 3; 8; 1; 4; 7; 10];;
let has_value = Trees.AVL.member 5 avl;;
let values = Trees.AVL.inorder avl;;
```

## Project Structure

```
/src
  /sorting        - Sorting algorithm implementations
  /trees          - Tree data structure implementations
  ods.ml          - Main library module
/test             - Test suite
```

## License

MIT License