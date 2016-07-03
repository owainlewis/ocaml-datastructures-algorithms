(* Leftist Trees *)

type 'a leftist =
  | Leaf
  | Node of 'a leftist * 'a * 'a leftist * int

let singleton k = Node (Leaf, k, Leaf, 1)

let rank tree =
  match tree with
    | Leaf -> 0
    | Node (_, _, _, n) -> n

let rec merge t1 t2 =
  match t1, t2 with
   | Leaf, t -> t | t, Leaf -> t
   | Node (l, k1, r, _), Node (_, k2, _, _) ->
       if k1 > k2 then merge t2 t1
       else
         let merged = merge r t2 in
         let rank_left = rank l
         and rank_right = rank merged in
         if rank_left >= rank_right then
           Node(l, k1, merged, rank_right+1)
         else
           Node(merged, k1, l, rank_left+1)

let insert x t = merge (singleton x) t

let get_min = function
  | Leaf -> None
  | Node (_, k, _, _) -> Some(k)

let delete_min = function
  | Leaf -> failwith "empty"
  | Node (l, _, r, _) -> merge l r
