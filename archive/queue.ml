module type QUEUE = sig
  exception EmptyQueue

  type 'a queue

  val empty : unit -> 'a queue
  (* Adds an item to the back of the queue *)
  val enqueue : 'a -> 'a queue -> 'a queue
  (* Take from the front and returns new queue *)
  val dequeue : 'a queue -> 'a queue
  val is_empty : 'a queue -> bool
  val dequeue: 'a queue -> 'a queue
  (* Peek *)
  val front : 'a queue -> 'a
end

module AppendListQueue : QUEUE = struct

  exception EmptyQueue

  type 'a queue = 'a list

  let empty() = []
  let enqueue v q = q @ [v]
  let deq = function
    | [] -> raise EmptyQueue
    | x::xs -> (x,xs)
  let dequeue q = snd (deq q)
  let is_empty = function
    | [] -> true
    | _  -> false

  let front q = fst (deq q)
end

(* (\** Need to implement module sig **\)
 * module MutableQueue = struct
 *
 *   exception EmptyQueue
 *
 *   type 'a queue = 'a ref
 *
 *   let q = ref []
 *   let empty() = q
 *   let enqueue v q = q := !q @ [v]
 *   let deq = function
 *     | [] -> raise EmptyQueue
 *     | x::xs -> (x,xs)
 *   let dequeue() = q := snd (deq !q)
 *   let front() = fst (deq !q)
 *
 *   let is_empty = function
 *     | [] -> true
 *     | _  -> false
 * end *)
