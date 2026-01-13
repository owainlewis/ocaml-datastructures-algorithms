(* Leftist Tree Implementation *)

(* A leftist tree is a heap-ordered binary tree that satisfies the leftist property:
   the rank of the left child is >= the rank of the right child.
   Rank is the length of the rightmost path to an empty node. *)

type 'a t =
  | Empty
  | Node of 'a t * 'a * 'a t * int  (* left, value, right, rank *)

let empty = Empty

let is_empty = function
  | Empty -> true
  | _ -> false

let singleton x = Node (Empty, x, Empty, 1)

let rank = function
  | Empty -> 0
  | Node (_, _, _, r) -> r

(* Merge two leftist trees *)
let rec merge t1 t2 =
  match t1, t2 with
  | Empty, t | t, Empty -> t
  | Node (l1, x1, r1, _), Node (_, x2, _, _) ->
    if x1 > x2 then
      merge t2 t1
    else
      let merged = merge r1 t2 in
      let rank_left = rank l1 in
      let rank_right = rank merged in
      if rank_left >= rank_right then
        Node (l1, x1, merged, rank_right + 1)
      else
        Node (merged, x1, l1, rank_left + 1)

let insert x t = merge (singleton x) t

let get_min = function
  | Empty -> None
  | Node (_, x, _, _) -> Some x

let find_min = function
  | Empty -> failwith "find_min: empty tree"
  | Node (_, x, _, _) -> x

let delete_min = function
  | Empty -> Empty
  | Node (l, _, r, _) -> merge l r

let extract_min = function
  | Empty -> failwith "extract_min: empty tree"
  | Node (l, x, r, _) -> (x, merge l r)

(* Build a leftist tree from a list *)
let of_list lst =
  List.fold_left (fun t x -> insert x t) Empty lst

(* Convert to sorted list by repeatedly extracting min *)
let to_sorted_list t =
  let rec aux acc = function
    | Empty -> List.rev acc
    | t ->
      let (min, rest) = extract_min t in
      aux (min :: acc) rest
  in
  aux [] t

(* Heapsort using leftist tree *)
let heapsort lst =
  to_sorted_list (of_list lst)
