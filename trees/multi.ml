(* Multiway trees *)

type 'a mtree = T of 'a * 'a mtree list

let t = T('a', [T('f',[T('g',[])]); T('c',[]); T('b',[T('d',[]); T('e',[])])])

(* Count the nodes in a multiway tree *)
let rec count_nodes (T(_, sub)) =
  let f = (fun n t -> n + count_nodes t)
  in List.fold_left f 1 sub

