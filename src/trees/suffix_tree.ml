(* Suffix Tree Implementation - Simple construction *)

module CharMap = Map.Make(Char)

type node = {
  mutable children: edge CharMap.t;
  mutable is_end: bool;
  id: int;
}

and edge = {
  label: string;
  target: node;
}

type t = {
  text: string;
  root: node;
  mutable node_count: int;
}

(* Create a new node *)
let create_node id = {
  children = CharMap.empty;
  is_end = false;
  id;
}

(* Find the longest common prefix of two strings *)
let common_prefix s1 s2 =
  let len = min (String.length s1) (String.length s2) in
  let rec aux i =
    if i >= len then i
    else if s1.[i] = s2.[i] then aux (i + 1)
    else i
  in
  aux 0

(* Insert a suffix into the tree *)
let rec insert_suffix tree node suffix =
  if String.length suffix = 0 then
    node.is_end <- true
  else
    let c = suffix.[0] in
    match CharMap.find_opt c node.children with
    | None ->
        (* No edge starting with this character, create one *)
        let new_node = create_node tree.node_count in
        tree.node_count <- tree.node_count + 1;
        new_node.is_end <- true;
        let edge = { label = suffix; target = new_node } in
        node.children <- CharMap.add c edge node.children
    | Some edge ->
        let prefix_len = common_prefix edge.label suffix in
        if prefix_len = String.length edge.label then
          (* Entire edge label matches, continue down the tree *)
          insert_suffix tree edge.target (String.sub suffix prefix_len (String.length suffix - prefix_len))
        else
          (* Need to split the edge *)
          let split_node = create_node tree.node_count in
          tree.node_count <- tree.node_count + 1;

          (* Edge from split_node to original target *)
          let remaining_label = String.sub edge.label prefix_len (String.length edge.label - prefix_len) in
          let old_edge = { label = remaining_label; target = edge.target } in
          split_node.children <- CharMap.add remaining_label.[0] old_edge CharMap.empty;

          (* Update the edge from parent to split_node *)
          let new_parent_edge = { label = String.sub edge.label 0 prefix_len; target = split_node } in
          node.children <- CharMap.add c new_parent_edge node.children;

          (* Insert the remaining suffix *)
          let remaining_suffix = String.sub suffix prefix_len (String.length suffix - prefix_len) in
          if String.length remaining_suffix = 0 then
            split_node.is_end <- true
          else
            insert_suffix tree split_node remaining_suffix

(* Create a new suffix tree from a string *)
let create text =
  let root = create_node 0 in
  let tree = { text; root; node_count = 1 } in

  (* Insert all suffixes *)
  for i = 0 to String.length text - 1 do
    let suffix = String.sub text i (String.length text - i) in
    insert_suffix tree root suffix
  done;

  tree

(* Check if a pattern exists in the text *)
let contains tree pattern =
  let rec search node remaining =
    if String.length remaining = 0 then
      true
    else
      let c = remaining.[0] in
      match CharMap.find_opt c node.children with
      | None -> false
      | Some edge ->
          let prefix_len = common_prefix edge.label remaining in
          if prefix_len = String.length remaining then
            (* Pattern is fully matched within or at end of this edge *)
            true
          else if prefix_len = String.length edge.label then
            (* Edge fully matched, continue to child *)
            search edge.target (String.sub remaining prefix_len (String.length remaining - prefix_len))
          else
            (* Mismatch within the edge *)
            false
  in
  search tree.root pattern

(* Find all starting positions of a pattern in the text *)
let find_all tree pattern =
  (* First, find the node where the pattern ends *)
  let rec find_pattern_node node remaining depth =
    if String.length remaining = 0 then
      Some (node, depth)
    else
      let c = remaining.[0] in
      match CharMap.find_opt c node.children with
      | None -> None
      | Some edge ->
          let prefix_len = common_prefix edge.label remaining in
          if prefix_len = String.length remaining then
            (* Pattern ends within this edge *)
            Some (edge.target, depth + prefix_len)
          else if prefix_len = String.length edge.label then
            (* Full edge match, continue *)
            find_pattern_node edge.target
              (String.sub remaining prefix_len (String.length remaining - prefix_len))
              (depth + prefix_len)
          else
            None
  in

  (* Collect all leaf positions under a node *)
  let rec collect_positions node current_depth acc =
    let acc' =
      if CharMap.is_empty node.children then
        (* Leaf node - calculate starting position *)
        (String.length tree.text - current_depth) :: acc
      else
        acc
    in
    CharMap.fold (fun _ edge acc ->
      collect_positions edge.target (current_depth + String.length edge.label) acc
    ) node.children acc'
  in

  match find_pattern_node tree.root pattern 0 with
  | None -> []
  | Some (node, depth) ->
      collect_positions node depth []

(* Visualize the suffix tree (for debugging) *)
let visualize tree =
  let rec visualize_node node depth =
    let indent = String.make (depth * 2) ' ' in
    Printf.printf "%sNode %d%s\n" indent node.id (if node.is_end then " [end]" else "");

    CharMap.iter (fun c edge ->
      Printf.printf "%s  '%c' -> \"%s\"\n" indent c edge.label;
      visualize_node edge.target (depth + 1)
    ) node.children
  in

  Printf.printf "Suffix tree for: \"%s\"\n" tree.text;
  visualize_node tree.root 0
