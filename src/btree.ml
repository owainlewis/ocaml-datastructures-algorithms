(* B-Tree Implementation *)

(* B-tree of order m has:
   - Each node can have at most m children
   - Each node (except root) has at least ceil(m/2) children
   - Root has at least 2 children unless it's a leaf
   - All leaves are at the same level
   - A non-leaf node with k children contains k-1 keys
*)

(* We'll implement a B-tree of order m where:
   - Each node has at most m-1 keys
   - Each node (except root) has at least ceil(m/2)-1 keys
   - The keys in each node are sorted
*)

type 'a entry = {
  key: 'a;
  value: 'a;  (* For simplicity, key and value have same type *)
}

type 'a node = {
  entries: 'a entry array;  (* Sorted array of entries *)
  mutable n_entries: int;   (* Number of entries currently in the node *)
  mutable children: 'a btree array option; (* None for leaf nodes *)
}

and 'a btree = 'a node option

(* B-tree with its order (m) *)
type 'a t = {
  order: int;
  mutable root: 'a btree;
}

(* Create a new empty B-tree of order m *)
let create order =
  if order < 3 then
    failwith "B-tree order must be at least 3";
  {
    order;
    root = None;
  }

(* Create a new empty node *)
let create_node order is_leaf =
  let node = {
    entries = Array.make (order - 1) {key = Obj.magic 0; value = Obj.magic 0};
    n_entries = 0;
    children = if is_leaf then None else Some(Array.make order None);
  } in
  Some(node)

(* Search for a key in the B-tree *)
let rec search btree key =
  let rec search_node = function
    | None -> None
    | Some node ->
        let i = ref 0 in
        (* Find the first entry with key >= the search key *)
        while !i < node.n_entries && key > node.entries.(!i).key do
          incr i
        done;
        
        if !i < node.n_entries && key = node.entries.(!i).key then
          (* Found the key *)
          Some node.entries.(!i).value
        else
          (* Not found in this node, check children if they exist *)
          match node.children with
          | None -> None  (* Leaf node, key not found *)
          | Some children -> search_node children.(!i)
  in
  search_node btree.root

(* Split a child node when it's full *)
let split_child parent_node i =
  match parent_node.children with
  | None -> failwith "Cannot split child of a leaf node"
  | Some children ->
      match children.(i) with
      | None -> failwith "Child node is None"
      | Some child_node ->
          let order = Array.length children in
          let t = order / 2 in
          
          (* Create new node for the right half *)
          let new_node = match create_node order (child_node.children = None) with
            | None -> failwith "Failed to create node"
            | Some n -> n
          in
          
          (* Copy the right half entries to the new node *)
          for j = 0 to t - 2 do
            new_node.entries.(j) <- child_node.entries.(j + t);
            new_node.n_entries <- new_node.n_entries + 1;
          done;
          
          (* Copy child pointers if not a leaf *)
          (match child_node.children, new_node.children with
           | Some child_children, Some new_children ->
               for j = 0 to t - 1 do
                 new_children.(j) <- child_children.(j + t);
               done;
           | _, _ -> ());
          
          (* Move the middle key to the parent *)
          for j = parent_node.n_entries downto i + 1 do
            parent_node.entries.(j) <- parent_node.entries.(j - 1);
          done;
          parent_node.entries.(i) <- child_node.entries.(t - 1);
          parent_node.n_entries <- parent_node.n_entries + 1;
          
          (* Update parent's children array *)
          for j = parent_node.n_entries downto i + 2 do
            children.(j) <- children.(j - 1);
          done;
          children.(i + 1) <- Some new_node;
          
          (* Adjust the original child node *)
          child_node.n_entries <- t - 1

(* Insert a key-value pair into a non-full node *)
let rec insert_non_full btree node key value =
  let i = ref (node.n_entries - 1) in
  
  match node.children with
  | None ->
      (* Leaf node, insert the key here *)
      while !i >= 0 && key < node.entries.(!i).key do
        node.entries.(!i + 1) <- node.entries.(!i);
        decr i;
      done;
      node.entries.(!i + 1) <- {key; value};
      node.n_entries <- node.n_entries + 1
  | Some children ->
      (* Find the child where the key should be inserted *)
      while !i >= 0 && key < node.entries.(!i).key do
        decr i;
      done;
      let child_idx = !i + 1 in
      
      match children.(child_idx) with
      | None -> failwith "Child node is None"
      | Some child ->
          (* Check if the child is full *)
          if child.n_entries = Array.length child.entries then (
            split_child node child_idx;
            (* After splitting, decide which child to go to *)
            if key > node.entries.(child_idx).key then
              incr child_idx
          );
          
          (* Recursively insert into the child *)
          match children.(child_idx) with
          | None -> failwith "Child node is None after split"
          | Some child -> insert_non_full btree child key value

(* Insert a key-value pair into the B-tree *)
let insert btree key value =
  match btree.root with
  | None ->
      (* Empty tree, create a root node *)
      let new_root = match create_node btree.order true with
        | None -> failwith "Failed to create root node"
        | Some n -> n
      in
      new_root.entries.(0) <- {key; value};
      new_root.n_entries <- 1;
      btree.root <- Some new_root
  | Some root ->
      if root.n_entries = btree.order - 1 then (
        (* Root is full, split it *)
        let new_root = match create_node btree.order false with
          | None -> failwith "Failed to create new root"
          | Some n -> n
        in
        
        match new_root.children with
        | None -> failwith "New root should have children array"
        | Some children ->
            children.(0) <- Some root;
            btree.root <- Some new_root;
            split_child new_root 0;
            
            (* Insert into the new root *)
            insert_non_full btree new_root key value
      ) else (
        (* Root is not full, insert directly *)
        insert_non_full btree root key value
      )

(* In-order traversal of the B-tree *)
let traverse btree =
  let result = ref [] in
  
  let rec traverse_node = function
    | None -> ()
    | Some node ->
        match node.children with
        | None ->
            (* Leaf node, just print the keys *)
            for i = 0 to node.n_entries - 1 do
              result := node.entries.(i) :: !result
            done
        | Some children ->
            (* Non-leaf node, traverse in-order *)
            for i = 0 to node.n_entries - 1 do
              traverse_node children.(i);
              result := node.entries.(i) :: !result
            done;
            traverse_node children.(node.n_entries)
  in
  
  traverse_node btree.root;
  List.rev !result