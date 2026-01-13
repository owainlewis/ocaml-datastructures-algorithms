(* Graph Algorithms *)

exception Vertex_not_found
exception Cyclic of string

(* Directed Graph using adjacency lists *)
module DiGraph = struct
  type 'a vertex = {
    value: 'a;
    mutable neighbors: 'a list;
  }

  type 'a t = ('a, 'a vertex) Hashtbl.t

  let create () : 'a t = Hashtbl.create 16

  let add_vertex g v =
    if not (Hashtbl.mem g v) then
      Hashtbl.add g v { value = v; neighbors = [] }

  let add_edge g src dest =
    add_vertex g src;
    add_vertex g dest;
    let vertex = Hashtbl.find g src in
    if not (List.mem dest vertex.neighbors) then
      vertex.neighbors <- dest :: vertex.neighbors

  let vertices g =
    Hashtbl.fold (fun k _ acc -> k :: acc) g []

  let neighbors g v =
    try (Hashtbl.find g v).neighbors
    with Not_found -> raise Vertex_not_found

  let has_vertex g v = Hashtbl.mem g v

  (* Depth-first search *)
  let dfs g start =
    let rec dfs_aux visited = function
      | [] -> List.rev visited
      | x :: xs ->
        if List.mem x visited then
          dfs_aux visited xs
        else
          let frontier = (neighbors g x) @ xs in
          dfs_aux (x :: visited) frontier
    in
    dfs_aux [] [start]

  (* Breadth-first search *)
  let bfs g start =
    let rec bfs_aux visited queue =
      match queue with
      | [] -> List.rev visited
      | x :: xs ->
        if List.mem x visited then
          bfs_aux visited xs
        else
          let frontier = xs @ (neighbors g x) in
          bfs_aux (x :: visited) frontier
    in
    bfs_aux [] [start]

  (* Build graph from list of edges *)
  let of_edges edges =
    let g = create () in
    List.iter (fun (src, dest) -> add_edge g src dest) edges;
    g
end

(* Topological sort for directed acyclic graphs *)
let topological_sort edges seed =
  let successors n =
    List.filter_map (fun (s, d) -> if s = n then Some d else None) edges
  in
  let rec sort path visited = function
    | [] -> visited
    | n :: nodes ->
      if List.mem n path then raise (Cyclic n)
      else
        let v' =
          if List.mem n visited then visited
          else n :: sort (n :: path) visited (successors n)
        in
        sort path v' nodes
  in
  sort [] [] [seed]

(* Simple edge-list based DFS *)
let dfs_edges edges start =
  let successors n =
    List.filter_map (fun (s, d) -> if s = n then Some d else None) edges
  in
  let rec dfs visited = function
    | [] -> List.rev visited
    | n :: nodes ->
      if List.mem n visited then
        dfs visited nodes
      else
        dfs (n :: visited) ((successors n) @ nodes)
  in
  dfs [] [start]
