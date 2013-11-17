module type ADJ = 
  sig
    type t
    (* A graph represented as a mutable list of vertices *)
    type graph
    (* A graph vertex in the form (v, incoming, outgoing) *)
    type vertex
    (* An edge in the form source -> dest -> weight *)
    type edge
    val create : unit -> graph
    val vertices : graph -> int list
    val is_empty : graph -> bool
    val add_vertex : graph -> int -> graph
    val find_vertex : graph -> int -> vertex option
  end

module Graph = struct
  exception VertexDoesNotExist
  type t = int
  type vertex = V of int * (int list ref) * (int list ref) 
  type edge   = vertex * vertex * int
  type graph  = vertex list ref
  let create () = ref []
  let vertices g = 
    List.map (fun (v) -> let V (x,_,_) = v in x) !g
  (* TODO Duplication of logic *)
  let out_func v = let V (_,x,_) = v in !x
  let in_func v  = let V (_,_,x) = v in !x
  let flatten_edges g f = 
    let edges = List.map f !g in 
    edges |> List.flatten
  let outgoing_edges g = flatten_edges g out_func
  let incoming_edges g = flatten_edges g in_func
  let is_empty g =
    match !g with
    | [] -> true
    | _  -> false
  (* In the following two functions vertex refers to the value not the type *)
  let find_vertex graph vertex =
    let rec find g v =
      match g with
      | [] -> None
      | V (x,_,_) as vtx :: xs -> if v = x then Some(vtx) else find xs v
    in find !graph vertex
  (* Core operations *)
  let add_vertex graph v = 
    let new_vertex = V (v, ref [], ref [])
    in graph := new_vertex :: !graph;
    graph

  (* Consider cost implications here as it's going to be a bit crappy *)
  let add_incoming_edge graph src dest = 
    let vtx = find_vertex graph src in
    match vtx with
      | Some(v) -> let V (_,i,_) = v in i := dest :: !i; v
      | None -> failwith "No matching vertex"

  let add_outgoing_edge graph src dest =
    let vtx = find_vertex graph src in
    match vtx with
    | Some(v) -> let V (_, _, o) = v in o := dest :: !o; v
    | None    -> failwith "No matching vertex"

  let add_undirected_edge graph src dest = 
    add_incoming_edge graph src dest;
    add_outgoing_edge graph src dest;
    graph;
end

