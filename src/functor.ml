(* OCaml Functors *)

module type SETSIG = sig
  type set
  type elt
  val empty  : set
  val member : elt -> set -> bool
  val add    : elt -> set -> set
  val find   : elt -> set -> elt
end

(* Abstraction for comparison *)

module type EQUALSIG = sig
  type t
  val equal : t -> t -> bool
end

(* The SET implementation. Define a Functor impmlementing the SETSIG signature *)

module MakeSet (Equal : EQUALSIG)
  : SETSIG with type elt = Equal.t =
struct
  open Equal
  type elt = t
  type set = elt list
  let empty = []
  let member x s = List.exists (equal x) s
  let add    x s = if member x s then s else x :: s
  let find   x s = List.find (equal x) s
end

(* In order to use MakeSet we need an implementation of a module with the EQUALSIG *)

(* This set will be able to compare on strings that are case insensitive *)

module StringNotCaseSensitive = struct
  type t = string
  let equal str1 str2 =
    String.lowercase str1 = String.lowercase str2
end

(* Implement the module *)

(* We can use this set abstraction to create and manipulate sets of strings,
   with case insensitive comparison of elements in a set. *)

module SSet = MakeSet (StringNotCaseSensitive)
let myset = SSet.add "Hello" SSet.empty;;
SSet.member "Hello" myset;; (* true *)

