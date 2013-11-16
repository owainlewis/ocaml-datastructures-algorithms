(* An list/array based binary heap structure *)

module type HEAP = sig
  val create: 'a list
end

module ListHeap : HEAP = struct
  let create = []
end

let h = ["";"T";"S";"R";"P";"N";"O";"A";"E";"I";"H";"G"]

(* Parent of node at k is at k/2. *)
let parent heap k = List.nth heap (k/2)

(* Children of node at k are at 2k and 2k+1 *)

let children heap k = 
  let lchild = List.nth heap (k*2) 
  and rchild = 
    let n = (k*2) + 1 in List.nth heap n
  in [lchild;rchild]

