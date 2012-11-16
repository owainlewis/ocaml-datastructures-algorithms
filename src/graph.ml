(* Graph algorithms *)

type 'a graph = { nodes : 'a list;  edges : ('a * 'a) list }

let digraph =
  { nodes = [1;2;3;4];
    edges = [(1,2); (2,4); (4,2); (4,1)] }



