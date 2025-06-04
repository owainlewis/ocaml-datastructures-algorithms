(* Trie Implementation *)

module CharMap = Map.Make(Char)

type trie = {
  is_end: bool;
  children: trie CharMap.t;
}

(* Create an empty trie *)
let empty = {
  is_end = false;
  children = CharMap.empty;
}

(* Insert a word into the trie *)
let insert word trie =
  let rec aux chars current =
    match chars with
    | [] -> { current with is_end = true }
    | c :: cs ->
        let child = 
          match CharMap.find_opt c current.children with
          | Some(t) -> aux cs t
          | None -> aux cs empty
        in
        { current with children = CharMap.add c child current.children }
  in
  aux (List.init (String.length word) (String.get word)) trie

(* Check if a word exists in the trie *)
let search word trie =
  let rec aux chars current =
    match chars with
    | [] -> current.is_end
    | c :: cs ->
        match CharMap.find_opt c current.children with
        | Some(child) -> aux cs child
        | None -> false
  in
  aux (List.init (String.length word) (String.get word)) trie

(* Check if any word with the given prefix exists in the trie *)
let starts_with prefix trie =
  let rec aux chars current =
    match chars with
    | [] -> true  (* Reached the end of prefix *)
    | c :: cs ->
        match CharMap.find_opt c current.children with
        | Some(child) -> aux cs child
        | None -> false
  in
  aux (List.init (String.length prefix) (String.get prefix)) trie

(* Get all words in the trie *)
let get_all_words trie =
  let rec aux current prefix acc =
    let acc' = if current.is_end then (prefix :: acc) else acc in
    CharMap.fold 
      (fun c child a -> aux child (prefix ^ String.make 1 c) a)
      current.children
      acc'
  in
  aux trie "" []

(* Delete a word from the trie *)
let delete word trie =
  let rec aux chars current path =
    match chars with
    | [] ->
        if not current.is_end then (current, false) else
        if not (CharMap.is_empty current.children) then
          ({ current with is_end = false }, false)
        else
          (empty, true) (* Remove this node *)
    | c :: cs ->
        match CharMap.find_opt c current.children with
        | None -> (current, false) (* Word not in trie *)
        | Some(child) ->
            let new_child, should_delete = aux cs child (c :: path) in
            if should_delete then
              if CharMap.cardinal current.children = 1 && not current.is_end then
                (empty, true)
              else
                ({ current with children = CharMap.remove c current.children }, false)
            else
              ({ current with children = CharMap.add c new_child current.children }, false)
  in
  let updated, _ = aux (List.init (String.length word) (String.get word)) trie [] in
  updated

(* Count the number of words in the trie *)
let count_words trie =
  let rec aux current count =
    let count' = if current.is_end then count + 1 else count in
    CharMap.fold 
      (fun _ child c -> aux child c)
      current.children
      count'
  in
  aux trie 0

(* Build a trie from a list of words *)
let build_from_list words =
  List.fold_left (fun t word -> insert word t) empty words

(* Auto-complete: find all words starting with a prefix *)
let autocomplete prefix trie =
  let rec find_prefix_node chars current =
    match chars with
    | [] -> Some current
    | c :: cs ->
        match CharMap.find_opt c current.children with
        | Some child -> find_prefix_node cs child
        | None -> None
  in
  
  let prefix_chars = List.init (String.length prefix) (String.get prefix) in
  match find_prefix_node prefix_chars trie with
  | None -> []
  | Some node -> 
      let words = get_all_words node in
      List.map (fun suffix -> prefix ^ suffix) words