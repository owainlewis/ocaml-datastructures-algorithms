(* A vertex or node of the graph *)
type vertex = V of (int * ((vertex * int) list ref)) * ((vertex * int) list ref)

(* An edge of the graph *)
type edge = vertex * vertex * int

(* A directed graph *)
type graph = (int ref * (vertex list) ref)

(* val create : unit -> graph *)
let create () = (ref 0, ref [])

(* return the vertices for a graph *)
let vertices (_, vlist) = !vlist

let add_vertex (c, vl) =
  incr c;


