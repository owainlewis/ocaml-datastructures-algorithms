(* OCaml Splay Tree *)

module type ORDERED_SET = sig
  type elem
end 

module SplayTree = 
struct
  type elem = int
  type t = Leaf | Node of t * elem * t
  type tree = t ref

  let empty () = ref Leaf

  let isEmpty t = 
    match !t with 
      Leaf -> true
    | _    -> false
end
