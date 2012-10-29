(* Stack data structure -> A polymorphic LIFO data structure *)

module type STACK = 
  sig
    type 'a stack
    exception EmptyStack
    val empty : 'a stack
    val is_empty : 'a stack -> bool
    val push : ('a * 'a stack) -> 'a stack
  end

module MutableStack : STACK =
  struct

  end
 
(* Think here about mutable vs non mutable. I.e do 
   a version of both one with mutating state
   and another without *)
module ImmutableStack = 
  struct
    (* Type signature *)
    type 'a stack = 'a list
    exception EmptyStack
    let empty : 'a stack = []
    let is_empty (l : 'a stack): bool = 
      match l with
         [] -> true
       | _ -> false
    (* Push an item onto the stack *)
    let push (item: 'a) (stack : 'a stack): 'a stack =
      item :: stack
    (* Push many items util function TODO fixme *)
    let push_many lst =
      let rec aux lst stack =
        match lst with
          [] -> stack
          | x::xs -> aux xs (push x stack) in
      aux lst Stack.empty
    (* Pop an element from a stack. Returns a new stack *)
    let pop (l:'a stack) : 'a stack = 
      match l with 
        [] -> raise EmptyStack
      | x::xs -> xs
  end
