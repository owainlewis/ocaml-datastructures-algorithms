(* A mutable LIFO stack using OCaml refs *)

module type MSTACK =
sig
  type 'a mstack
  val empty : unit -> 'a mstack
  val push : 'a mstack -> 'a -> unit
  val pop : 'a mstack -> 'a option
  val top : 'a mstack -> 'a option
  val get : 'a mstack -> 'a list
end

(* My first attempt *)

module MutableStack : MSTACK = struct

  type 'a mstack = ('a list) ref

  let empty(): 'a mstack = ref []

  let get(stack: 'a mstack) = !stack

  (* update the ref and push and item *)
  let push(stack: 'a mstack) (item: 'a): unit =
    stack := item::(!stack)

  let is_empty(stack: 'a mstack) = if !stack == [] then true else false

  let pop(stack: 'a mstack): 'a option =
    match (!stack) with
      [] -> None
    | x::xs -> (stack := xs); Some x

  let top(stack: 'a mstack) =
    let items = !stack in
    List.hd items
end

module MS = struct

  type 'a t = { mutable c : 'a list }

  exception Empty

  let create () = { c = [] }
  let push x stack = stack.c <- x :: stack.c

  let pop stack =
    let s = stack.c in
    match s with
    | x::xs -> stack.c <- xs; x
    | [] -> raise Empty

  let top s =
    match s.c with
      x::xs -> x
    | [] -> raise Empty

  let is_empty stack = stack.c = []
  let size stack = let s = stack.c in List.length s
end

