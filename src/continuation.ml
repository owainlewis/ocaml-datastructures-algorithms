(**************************

Programming continuations
(might be worth looking at scheme for this stuff?

**************************)

let rec sum_recur (s: int list): int =
  match s with
    [] -> 0
  | x::xs -> x + sum_recur xs

(* no deferred operations on any recursive call *)
let sum_tail_recur (s: int list): int =
  let rec aux' s a =
    match s with
      [] -> a
    | x::xs -> aux' xs (a + x) in
    aux' s 0

(* create a function that packages up deferred operations,
   then passes it down in the recursive call for somebody else to perform *)
let sum_continutation (s: int list): int =
  let rec sum' s k =
    match s with
      [] -> k 0
    | x::xs -> sum' xs (fun a -> k (x + a)) in
  sum' s (fun x -> x)

(* Implementing a fold using continuations *)
let rec fold_right_recur (f : 'a -> 'b -> 'b) (s : 'a list) (b : 'b) : 'b =
  match s with
    [] -> b
  | x::xs -> f x (fold_right_recur f xs b)

(* using continuations *)
let fold_right_continuation (f : 'a -> 'b -> 'b) (s : 'a list) (b : 'b) : 'b =
  let identity = (fun x -> x) in (* the identity function *)
  let rec fold_right' s k =
    match s with
      [] -> k b
    | x::xs -> fold_right' xs (fun y -> k (f x y)) in
    fold_right' s identity

