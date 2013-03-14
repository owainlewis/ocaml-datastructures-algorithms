(* Random module kata *)

module ModuleName =
struct
end

(* Sigs *)

module type PERSONSIG =
sig
end


module Person : PERSONSIG = struct
end

(* Sets with union and intersection
 * mem x (add x S) = true
 * mem x (rem x S) = false
 * mem x (union S1 S2) = (mem x S1) || (mem x S2)
 * mem x (inter S1 S2) = (mem x S1) && (mem x S2)
 *)

module type SETSIG = sig
  type 'a set
  val empty : 'a set
  val add : 'a -> 'a set -> 'a set
  val mem : 'a -> 'a set -> bool
  val rem : 'a -> 'a set -> 'a set
  val union: 'a set -> 'a set -> 'a set
  val inter: 'a set -> 'a set -> 'a set
end

module Set : SETSIG = struct
  type 'a set = 'a list
  let empty = []
  let add x l = x :: l
  let mem x l = List.mem x l
  let rem x l = List.filter (fun h -> h <> x) l
  let union l1 l2 = l1 @ l2
  let inter l1 l2 = List.filter (fun h -> List.mem h l2) l1
end

module type ISETSIG = sig
  type 'a set
  val empty : 'a set
  val add   : 'a -> 'a set -> 'a set
end

module ISet : ISETSIG = struct
  type 'a set = 'a list
  let empty = []
  (* Check if the member already exists *)
  let add x s = x :: s
end
