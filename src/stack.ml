(* Stack data structure -> A polymorphic LIFO data structure *)

module type STACK = 
  sig
    type 'a stack
    exception EmptyStack
    val empty : 'a stack
    val isEmpty : 'a stack -> bool
    val push : ('a * 'a stack) -> 'a stack
    (*val pop : 'a stack -> 'a stack*)
  end
 
module Stack = 
  struct
    (* Type signature *)
    type 'a stack = 'a list
    exception EmptyStack
    let empty : 'a stack = []
    let isEmpty (l : 'a stack): bool = 
      match l with
         [] -> true
       | _ -> false
    (* Push an item onto the stack *)
    let push (item: 'a) (stack : 'a stack): 'a stack =
      item :: stack
    (* Push many items util function TODO fixme *)
    let pushMany lst stack =
      match lst with
        [] -> lst
        | x::xs -> pushMany xs (push x)
  end
