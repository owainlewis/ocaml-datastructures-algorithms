(* B-Tree Implementation *)

(* B-tree of order t (minimum degree) has:
   - Every node has at most 2t-1 keys
   - Every node (except root) has at least t-1 keys
   - All leaves are at the same level
*)

type 'a entry = {
  key: 'a;
  value: 'a;
}

type 'a node = {
  mutable keys: 'a entry list;
  mutable children: 'a node list;
  mutable is_leaf: bool;
}

type 'a t = {
  mutable root: 'a node option;
  min_degree: int;  (* t: minimum degree *)
}

let create_node is_leaf = {
  keys = [];
  children = [];
  is_leaf;
}

let create order =
  if order < 2 then
    failwith "B-tree minimum degree must be at least 2";
  { root = None; min_degree = order }

(* Search for a key in the B-tree *)
let search btree key =
  let rec search_node node =
    (* Find the first key >= search key *)
    let rec find_idx keys idx =
      match keys with
      | [] -> idx
      | entry :: rest ->
          if key <= entry.key then idx
          else find_idx rest (idx + 1)
    in
    let i = find_idx node.keys 0 in

    (* Check if we found the key *)
    let found =
      match List.nth_opt node.keys i with
      | Some entry when entry.key = key -> Some entry.value
      | _ -> None
    in

    match found with
    | Some v -> Some v
    | None ->
        if node.is_leaf then None
        else
          match List.nth_opt node.children i with
          | Some child -> search_node child
          | None -> None
  in
  match btree.root with
  | None -> None
  | Some root -> search_node root

(* Split a full child node *)
let split_child parent i child t =
  let new_node = create_node child.is_leaf in

  (* The middle key will go up to the parent *)
  let mid = t - 1 in

  (* Split the keys: left gets [0, mid), middle goes to parent, right gets (mid, end] *)
  let left_keys = List.filteri (fun j _ -> j < mid) child.keys in
  let mid_key = List.nth child.keys mid in
  let right_keys = List.filteri (fun j _ -> j > mid) child.keys in

  new_node.keys <- right_keys;
  child.keys <- left_keys;

  (* Split children if not a leaf *)
  if not child.is_leaf then begin
    let left_children = List.filteri (fun j _ -> j <= mid) child.children in
    let right_children = List.filteri (fun j _ -> j > mid) child.children in
    new_node.children <- right_children;
    child.children <- left_children
  end;

  (* Insert middle key into parent and new child into parent's children *)
  let new_parent_keys =
    List.filteri (fun j _ -> j < i) parent.keys @
    [mid_key] @
    List.filteri (fun j _ -> j >= i) parent.keys
  in
  let new_parent_children =
    List.filteri (fun j _ -> j <= i) parent.children @
    [new_node] @
    List.filteri (fun j _ -> j > i) parent.children
  in
  parent.keys <- new_parent_keys;
  parent.children <- new_parent_children

(* Insert into a non-full node *)
let rec insert_non_full node key value t =
  if node.is_leaf then begin
    (* Insert key in sorted order *)
    let rec insert_sorted keys =
      match keys with
      | [] -> [{key; value}]
      | entry :: rest ->
          if key < entry.key then {key; value} :: entry :: rest
          else if key = entry.key then {key; value} :: rest  (* Update existing *)
          else entry :: insert_sorted rest
    in
    node.keys <- insert_sorted node.keys
  end else begin
    (* Find child to insert into *)
    let rec find_child_idx keys idx =
      match keys with
      | [] -> idx
      | entry :: rest ->
          if key < entry.key then idx
          else find_child_idx rest (idx + 1)
    in
    let i = find_child_idx node.keys 0 in

    match List.nth_opt node.children i with
    | None -> failwith "Missing child"
    | Some child ->
        (* If child is full, split it first *)
        if List.length child.keys = 2 * t - 1 then begin
          split_child node i child t;
          (* After split, determine which child to go to *)
          let target_idx =
            match List.nth_opt node.keys i with
            | Some entry when key > entry.key -> i + 1
            | _ -> i
          in
          match List.nth_opt node.children target_idx with
          | Some target -> insert_non_full target key value t
          | None -> failwith "Missing child after split"
        end else
          insert_non_full child key value t
  end

(* Insert a key-value pair into the B-tree *)
let insert btree key value =
  let t = btree.min_degree in
  match btree.root with
  | None ->
      let node = create_node true in
      node.keys <- [{key; value}];
      btree.root <- Some node
  | Some root ->
      if List.length root.keys = 2 * t - 1 then begin
        (* Root is full, create new root and split *)
        let new_root = create_node false in
        new_root.children <- [root];
        btree.root <- Some new_root;
        split_child new_root 0 root t;
        insert_non_full new_root key value t
      end else
        insert_non_full root key value t

(* In-order traversal of the B-tree *)
let traverse btree =
  let rec traverse_node node =
    if node.is_leaf then
      node.keys
    else
      let rec interleave keys children acc =
        match keys, children with
        | [], [] -> List.rev acc
        | [], [child] -> List.rev_append acc (traverse_node child)
        | key :: krest, child :: crest ->
            let child_entries = traverse_node child in
            interleave krest crest (key :: List.rev_append child_entries acc)
        | _ -> List.rev acc
      in
      interleave node.keys node.children []
  in
  match btree.root with
  | None -> []
  | Some root -> traverse_node root
