(* Multiway trees *)

module MultiwayTree = struct

end

type 'a mult_tree = T of 'a * 'a mult_tree list

(* TODO *)
let make_tree xs = xs

let t = T('a', 
          [T('f', 
            [T('g',[])]); T('c',[]); T('b', 
                                       [T('d',[]); T('e',[])])])

let rec count_tree_nodes (T(_, sub)) =
  List.fold_left (fun n t -> n + count_tree_nodes t) 1 sub


