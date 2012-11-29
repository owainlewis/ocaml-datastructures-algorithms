(* Graph algorithms *)

type 'a graph = { nodes : 'a list;  edges : ('a * 'a) list }

let make_graph n e  =
  { nodes = n;
    edges = e }

let digraph =
  { nodes = [1;2;3;4];
    edges = [(1,2); (2,4); (4,2); (4,1)] }

let contains l e =
  let rec aux l e =
    if l == e then
      true
    else 
      aux List.tl e
  in aux l e

(* TODO has_node? find connected nodes find edge *)  
