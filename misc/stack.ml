(* Stack data structure -> A polymorphic LIFO data structure *)

module type ASTACK = 
  sig
    type 'a stack
    exception EmptyStack
    val empty : 'a stack
    val isEmpty : 'a stack -> bool
    val push : ('a * 'a stack) -> 'a stack
    val pop : 'a stack -> 'a stack
    val top : 'a stack -> 'a
    val map : ('a -> 'b) -> 'a stack -> 'b stack
    val app :  ('a -> unit) -> 'a stack -> unit
  end

module IStack : ASTACK = 
  struct
    type 'a stack = 'a list

    exception EmptyStack

    let empty : 'a stack = []

    let isEmpty (l:'a list): bool = 
      (match l with
         [] -> true
       | _ -> false)

    let push ((x:'a), (l:'a stack)):'a stack = x::l

    let pop (l:'a stack):'a stack = 
      (match l with 
         [] -> raise EmptyStack
       | (x::xs) -> xs)

    let top (l:'a stack):'a = 
      (match l with
         [] -> raise EmptyStack
       | (x::xs) -> x)

    let map (f:'a -> 'b) (l:'a stack):'b stack = List.map f l

    let app (f:'a -> unit) (l:'a stack):unit = List.iter f l
  end

