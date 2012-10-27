(* Graph algorithms *)

type Vertex = int
type Graph = (vertex * vertex list) list

val graph = [(1, [2, 4]), (2, [3]), (3, []), 
            (4, [3, 6]), (5, [4, 8]), (6, [5, 8]), 
            (7, []), (8, []), (9, [8])]
