(* Graph Algorithms *)

(* Set would be a better choice here for when I get some time *)
module DiGraph = struct
  exception VertexDoesNotExist
  exception Cyclic of string
  (* TODO parameterize this module *)
  type t = string
  type vertex = V of t * (t list ref)
  type graph = vertex list ref 
  let create() = ref []
  let ident v = let V (x, _) = v in x
  let vertices g = List.map ident !g  
  let has_vertex g v = List.mem v (vertices g)
  let add_vertex g v = 
    if has_vertex g v then g
    else
      let new_vertex = V (v, ref []) in
      g := new_vertex :: !g;
      g
  let get_vertex g v =
    let rec aux vert_list vertex =
      match vert_list with
      | [] -> None
      | x::xs -> if (ident x) = vertex then Some(x) else aux xs vertex
    in aux !g v
  (* Adds a ONE-WAY connection. For undirected the operation needs to be done
     in both directions *)
  let add_edge g src dest =
    add_vertex g src;
    add_vertex g dest;
    match (get_vertex g src) with
    | Some(v) -> let V (_, adjList) = v in adjList := dest :: !adjList
    (* Todo in theory we can't reach this case *)
    | _ -> failwith "Source vertex does not exist"
  let successors g v =
    let vtx = get_vertex g v in
    match vtx with
    | Some(vertex) -> let V (_, adjList) = vertex in !adjList
    | None -> raise VertexDoesNotExist
  (* Builds a directed graph from a list of edge pairs i.e [(1,2);(2,3)] etc *)
  let build_directed_graph pairs = 
    let g = create() in
    List.map (fun (src, dest) -> add_edge g src dest) pairs;
    g

  let sample_graph = 
    let edges = [
      ("a", "b"); ("a", "c");
      ("a", "d"); ("b", "e");
      ("c", "f"); ("d", "e");
      ("e", "f"); ("e", "g") ]
    in build_directed_graph edges

  let dfs graph start_state =
    let rec depth_first_search graph visited = function
        [] -> List.rev visited
      | x::xs ->
          if List.mem x visited then
            dfs graph visited xs
          else 
            let frontier = (successors graph x) @ xs
            in dfs graph (x::visited) frontier
    in depth_first_search graph [] (start_state::[])
end

let edges = [
  ("a", "b"); ("a", "c");
  ("a", "d"); ("b", "e");
  ("c", "f"); ("d", "e");
  ("e", "f"); ("e", "g") ]

let successors n edges =
  let matching (s,_) = s = n in
    List.map snd (List.filter matching edges)

let rec dfs edges visited = function
    [] -> List.rev visited
  | n::nodes ->
    if List.mem n visited then
      dfs edges visited nodes
    else dfs edges (n::visited) ((successors n edges) @ nodes)

exception Cyclic of string
let topological_sort edges seed =
  let rec sort path visited = function
      [] -> visited
    | n::nodes ->
      if List.mem n path then raise (Cyclic n) else
      let v' = if List.mem n visited then visited else
        n :: sort (n::path) visited (successors n edges)
      in sort path v' nodes
  in sort [] [] [seed]

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

