(* An list/array based binary heap structure *)

module type HEAP = sig
  val create: 'a list
end

module ListHeap : HEAP = struct
  let create() = [0]
  (* Parent of node at k is at k/2. *)
  let parent_index heap k =
    List.nth heap (k/2)

  let left_child_index heap k =
    List.nth heap (k*2)

  let right_child_index heap k =
    let i = k*2 in List.nth heap (i + 1)

  (* Children of node at k are at 2k and 2k+1 *)
  let children heap k =
    let l_index = ListHeap.left_child_index(heap,k)
    and r_index = ListHeap.right_child_index(heap,k)
    in [l_index;r_index]
end

let h = ["";"T";"S";"R";"P";"N";"O";"A";"E";"I";"H";"G"]
