(* A mutable stack using OCaml refs *)

module type MSTACK =
sig
  (* A mutable stack of alpha *)
  type 'a mstack
  (* Returns an empty stack *)
  val empty : unit -> 'a mstack
  (* Push an item on to the stack. Because it's mutable returns nothing *)
  val push : 'a mstack -> 'a -> unit
  (* Pop the head  *)
  val pop : 'a mstack -> 'a option
end

module MutableStack : MSTACK =
  struct
    type 'a mstack = ('a list) ref

    let empty() : 'a mstack = ref []

    let push(stack: 'a mstack) (item: 'a) : unit =
      stack := item::(!stack)

    let pop(stack: 'a mstack) : 'a option =
      match (!stack) with
        [] -> None
      | x::xs -> (stack := xs); Some x
  end

