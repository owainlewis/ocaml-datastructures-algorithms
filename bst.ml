
module Bst = struct 
  type t = int
  type bst = 
    Empty 
  | Node of t * bst * bst 
 
   let empty() = Empty (* empty binary search tree *) 
 
   let is_empty = function (* return true for empty bst *) 
     Empty -> true 
   | Node (_, _, _) -> false 
 
  let rec insert n = function (* insert n into binary search tree *) 
     Empty -> Node (n, Empty, Empty) 
  | Node (m, left, right) -> 
      if m = n then Node (m, left, right) 
      else if n < m then Node(m, (insert n left), right) 
      else Node(m, left, (insert n right)) 

  (* Implement the following functions (also do a delete min operation!
     val min : bst -> int 
     val fold : ('a -> int -> 'a) -> 'a -> bst -> 'a 
     val size : bst -> int 
   *) 

  let rec min = function
    Empty -> None
  | Node (_, left, _) -> 
      match left with
        (* base case *)
        Node(v, Empty, Empty) -> Some(v)
      | Node(_, l, _) -> min(l)

end 
