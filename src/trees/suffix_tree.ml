(* Suffix Tree Implementation - Ukkonen's Algorithm *)

module CharMap = Map.Make(Char)

type node = {
  mutable suffix_link: node option;
  mutable children: edge CharMap.t;
  id: int;
}

and edge = {
  start: int;
  mutable end_pos: int ref;
  target: node;
}

type t = {
  text: string;
  root: node;
  mutable node_count: int;
}

(* Create a new node *)
let create_node ~suffix_link ~id = {
  suffix_link;
  children = CharMap.empty;
  id;
}

(* Create a new suffix tree *)
let create text =
  let root = create_node ~suffix_link:None ~id:0 in
  let tree = { text; root; node_count = 1 } in
  
  let global_end = ref (-1) in
  
  (* Helper function to get character at position *)
  let char_at pos = text.[pos] in
  
  (* Helper function to add a new edge *)
  let add_edge parent_node start_pos end_pos target_node =
    let c = char_at start_pos in
    let edge = { start = start_pos; end_pos; target = target_node } in
    parent_node.children <- CharMap.add c edge parent_node.children
  in
  
  (* Traverse from a node following the given substring *)
  let rec traverse node str_pos end_pos =
    if str_pos > end_pos then
      (node, str_pos) (* Reached the end of the string *)
    else
      let c = char_at str_pos in
      match CharMap.find_opt c node.children with
      | None -> (node, str_pos) (* No path to follow *)
      | Some edge ->
          let edge_len = !(edge.end_pos) - edge.start + 1 in
          let str_len = end_pos - str_pos + 1 in
          
          if str_len < edge_len then
            (* String ends within this edge *)
            (node, str_pos)
          else
            (* Continue traversal from the target node *)
            traverse edge.target (str_pos + edge_len) end_pos
  in
  
  (* Find the end of the matching prefix on an edge *)
  let find_match_end edge_start str_pos str_end =
    let rec match_chars i j =
      if i > str_end || j > !(global_end) then
        i - str_pos (* Length of match *)
      else if char_at i = char_at j then
        match_chars (i + 1) (j + 1)
      else
        i - str_pos (* Length of match before mismatch *)
    in
    match_chars str_pos edge_start
  in
  
  (* Split an edge at the given position *)
  let split_edge parent edge split_pos =
    let new_node = create_node ~suffix_link:None ~id:(tree.node_count) in
    tree.node_count <- tree.node_count + 1;
    
    let new_edge_start = edge.start + split_pos in
    
    (* Update parent's child pointer *)
    let c = char_at edge.start in
    let new_parent_edge = { 
      start = edge.start; 
      end_pos = ref (new_edge_start - 1); 
      target = new_node 
    } in
    parent.children <- CharMap.add c new_parent_edge parent.children;
    
    (* Add edge from new node to old target *)
    let c = char_at new_edge_start in
    let new_child_edge = { 
      start = new_edge_start; 
      end_pos = edge.end_pos; 
      target = edge.target 
    } in
    new_node.children <- CharMap.add c new_child_edge CharMap.empty;
    
    new_node
  in
  
  (* Phase i: add all suffixes of text[0..i] *)
  let rec phase i =
    global_end := i;
    
    let rec extend_phase j =
      if j > i then ()
      else
        let (origin, start_pos) = traverse tree.root j i in
        
        if start_pos > i then
          (* This suffix is already in the tree *)
          extend_phase (j + 1)
        else
          (* Need to add this suffix *)
          if start_pos = j then
            (* Suffix starts at root *)
            let new_leaf = create_node ~suffix_link:None ~id:(tree.node_count) in
            tree.node_count <- tree.node_count + 1;
            add_edge origin start_pos (ref i) new_leaf;
            extend_phase (j + 1)
          else
            (* Suffix starts on an edge *)
            let c = char_at start_pos in
            let edge = CharMap.find c origin.children in
            let match_len = find_match_end edge.start start_pos i in
            
            if match_len = i - start_pos + 1 then
              (* Suffix is already in the tree *)
              extend_phase (j + 1)
            else
              (* Split the edge and add a new leaf *)
              let split_node = split_edge origin edge match_len in
              let new_leaf = create_node ~suffix_link:None ~id:(tree.node_count) in
              tree.node_count <- tree.node_count + 1;
              add_edge split_node (start_pos + match_len) (ref i) new_leaf;
              extend_phase (j + 1)
    in
    
    extend_phase 0;
    if i + 1 < String.length text then
      phase (i + 1)
  in
  
  (* Start building the tree *)
  if String.length text > 0 then
    phase 0;
  
  tree

(* Check if a pattern exists in the text *)
let contains tree pattern =
  let rec traverse node pattern_pos =
    if pattern_pos >= String.length pattern then
      true
    else
      let c = pattern.[pattern_pos] in
      match CharMap.find_opt c node.children with
      | None -> false
      | Some edge ->
          let edge_len = !(edge.end_pos) - edge.start + 1 in
          let remaining_len = String.length pattern - pattern_pos in
          
          if remaining_len <= edge_len then
            (* Pattern ends on this edge, check if it matches *)
            let rec check_match i j count =
              if count = 0 then true
              else if i >= String.length tree.text || j >= String.length pattern then
                false
              else if tree.text.[i] = pattern.[j] then
                check_match (i + 1) (j + 1) (count - 1)
              else
                false
            in
            check_match edge.start pattern_pos remaining_len
          else
            (* Follow the edge and continue matching *)
            let rec check_edge i j count =
              if count = 0 then
                traverse edge.target (pattern_pos + edge_len)
              else if tree.text.[i] <> pattern.[j] then
                false
              else
                check_edge (i + 1) (j + 1) (count - 1)
            in
            check_edge edge.start pattern_pos edge_len
  in
  
  traverse tree.root 0

(* Find all occurrences of a pattern in the text *)
let find_all tree pattern =
  let rec traverse node pattern_pos path_start =
    if pattern_pos >= String.length pattern then
      (* Found a match, collect all leaf positions below this node *)
      let rec collect_leaves node acc =
        if CharMap.is_empty node.children then
          (* This is a leaf, add its position *)
          (path_start - pattern_pos) :: acc
        else
          (* Collect leaves from all children *)
          CharMap.fold 
            (fun _ edge acc -> collect_leaves edge.target acc) 
            node.children 
            acc
      in
      collect_leaves node []
    else
      (* Continue matching the pattern *)
      let c = pattern.[pattern_pos] in
      match CharMap.find_opt c node.children with
      | None -> []
      | Some edge ->
          let edge_len = !(edge.end_pos) - edge.start + 1 in
          let remaining_len = String.length pattern - pattern_pos in
          
          if remaining_len <= edge_len then
            (* Pattern ends on this edge, check if it matches *)
            let matches = ref true in
            for i = 0 to remaining_len - 1 do
              if pattern.[pattern_pos + i] <> tree.text.[edge.start + i] then
                matches := false
            done;
            
            if !matches then
              (* Continue to collect all leaves *)
              let new_node = 
                if remaining_len = edge_len then edge.target
                else node
              in
              traverse new_node (pattern_pos + remaining_len) (path_start + remaining_len)
            else
              []
          else
            (* Need to match the entire edge first *)
            let matches = ref true in
            for i = 0 to edge_len - 1 do
              if pattern.[pattern_pos + i] <> tree.text.[edge.start + i] then
                matches := false
            done;
            
            if !matches then
              traverse edge.target (pattern_pos + edge_len) (path_start + edge_len)
            else
              []
  in
  
  traverse tree.root 0 0

(* Visualize the suffix tree (for debugging) *)
let visualize tree =
  let rec visualize_node node depth =
    let indent = String.make (depth * 2) ' ' in
    Printf.printf "%sNode %d\n" indent node.id;
    
    CharMap.iter (fun c edge ->
      let edge_str = String.sub tree.text edge.start (!(edge.end_pos) - edge.start + 1) in
      Printf.printf "%s  %c -> \"%s\" (pos: %d-%d)\n" 
        indent c edge_str edge.start !(edge.end_pos);
      visualize_node edge.target (depth + 1)
    ) node.children
  in
  
  visualize_node tree.root 0