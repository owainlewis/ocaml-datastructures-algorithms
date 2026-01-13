(* Priority Queue Implementation using a heap *)

exception Empty_queue

type 'a t =
  | Empty
  | Node of int * 'a * 'a t * 'a t  (* priority, value, left, right *)

let empty = Empty

let is_empty = function
  | Empty -> true
  | _ -> false

let rec insert priority value pq =
  match pq with
  | Empty -> Node (priority, value, Empty, Empty)
  | Node (p, v, left, right) ->
    if priority <= p then
      Node (priority, value, insert p v right, left)
    else
      Node (p, v, insert priority value right, left)

let rec remove_top = function
  | Empty -> raise Empty_queue
  | Node (_, _, left, Empty) -> left
  | Node (_, _, Empty, right) -> right
  | Node (_, _, (Node (lp, lv, _, _) as left), (Node (rp, rv, _, _) as right)) ->
    if lp <= rp then
      Node (lp, lv, remove_top left, right)
    else
      Node (rp, rv, left, remove_top right)

let extract = function
  | Empty -> raise Empty_queue
  | Node (priority, value, _, _) as pq ->
    (priority, value, remove_top pq)

let peek = function
  | Empty -> raise Empty_queue
  | Node (priority, value, _, _) -> (priority, value)

let of_list lst =
  List.fold_left (fun pq (p, v) -> insert p v pq) Empty lst
