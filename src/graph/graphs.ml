(* A representation of directed graphs with integer weighted edges, in
 * terms of adjacency lists of vertices, explicitly representing both
 * the outgoing edge list and the incoming edge at each vertex.
 *
 * A graph is represented as a pair of counter of the number of
 * vertices and a list of vertices.
 *
 * A vertex is represented as a triple of a unique integer ID, an
 * outgoing list and an incoming list.  The outgoing list is a
 * list of pairs of destination vertex and weight.  The incoming vertex
 * list is a list of pairs of source vertex and weight.  Note that as
 * each edge is stored twice, incoming and outgoing, it must be
 * consistent in the two lists (stored in the out-list of the source
 * vertex and the in-list of the destination vertex).
 *
 * An edge is a triple of a source vertex, destination vertex and
 * weight, and is constructed on the fly from the vertex out lists rather
 * than being stored in the data structure.
 *
 * Vertex comparison is done based on equality of the (unique) integer
 * ids.  Note that structural equality comparison is not viable here as
 * this is a highly circular structure in general.  *)

  type vertex = V of (int * ((vertex * int) list ref) * ((vertex * int) list ref))
  type edge = vertex * vertex * int
  type graph = (int ref * (vertex list) ref)

  let compare (V (id1, _, _)) (V (id2, _, _)) = compare id1 id2

  let create () = (ref 0, ref [])

  let is_empty (c, _) = !c = 0

  let vertices (_, vl) = !vl

  let num_vertices (c, _) = !c

  let edge_info e = e

  let vertex_id (V (id, _,  _)) = id

  let outgoing src =
    let V (_, ol, _) = src in
    List.map (fun (dst, w) -> (src, dst, w)) !ol

  let incoming_rev revsrc =
    let V (_, _, il) = revsrc in
    List.map (fun (revdst, w) -> (revsrc, revdst, w)) !il

  let incoming dst =
    let V (_, _, il) = dst in
    List.map (fun (src, w) -> (src, dst, w)) !il

  let out_degree (V (_, ol, _)) = List.length !ol

  let in_degree (V (_, _, il)) = List.length !il

  let edges (_, vl) = List.flatten (List.map outgoing !vl)

  let add_vertex (c, vl) =
    incr c;
    let v = V (!c, ref [], ref []) in
    vl := v :: !vl;
    v

  (* When adding and removing edges, dst goes in out-list of src
   * and src goes in in-list of dst *)
  let add_edge (src, dst, w) =
    let V (id1, ol, _), V (id2, _, il) = src, dst in
    ol := (dst, w) :: !ol;
    il := (src, w) :: !il

  let remove_edge (src, dst) =
    let V (_, ol, _), V (_, _, il) = src, dst in
    ol := List.remove_assoc dst !ol;
    il := List.remove_assoc src !il

  let remove_vertex g v =
    match v with V (id, ol, il) ->
      (* Remove v from in-list of all vertices on its out-list and
         from out-list of all vertices on its in-list.  *)
      (List.iter (fun (dst, w) ->
        match dst with
      V (_, _, il') -> il' := List.remove_assoc v !il') !ol;
       List.iter (fun (src, w) ->
        match src with
      V(_, ol', _) -> ol' := List.remove_assoc v !ol') !il;
       (* Make sure that if someone has a handle on the vertex there are no edges
          to follow into the graph that this is no longer a part of. *)
       ol := [];
       il := [];
       (* Decrease vertex count and remove from vertex list. *)
       match g with (c, vl) ->
         c := !c - 1;
         vl := List.filter (function (V (id', _, _)) -> id' <> id) !vl)

  (* To copy a graph, create a map from vertices of the old graph to
   * vertices of the new graph, then iterate over edges of the old
   * graph adding edges between corresponding vertices of the new graph
   * (using the map). *)

  module VMap = Map.Make (struct type t = vertex let compare = compare end)

  let copy g =
    let map = ref VMap.empty in
    let ng = (ref 0, ref []) in
    match g with
      (_, vl) ->
        match ng with
          (nc,nvl) ->
            (* Create new vertices and add to map from old
             * vertices, ensuring corresponding vertices have same
             * id number. *)
             (List.iter
               (function v ->
                  match v with V (id, _, _) ->
                    (nc := !nc + 1;
                    nvl := V (id, ref [], ref []) :: !nvl;
                    map := VMap.add v (List.hd !nvl) !map))
              !vl;
       (* Iterate over old edges, for each creating
        * corresponding new edge by looking up new
        * vertices in map. *)
       List.iter
         (function e ->
            match e with
              (src, dst, w) ->
                 let nsrc = VMap.find src !map
                 and ndst = VMap.find dst !map in
                 (match nsrc with
                    V (_, ol, _) -> ol := (ndst, w) :: !ol;
                    match ndst with
                      V(_, _, il) -> il := (nsrc, w) :: !il))
         (edges g);
       ng)

(* Sets of vertices, used in traversing from a node via DFS or BFS *)
module VSet = Set.Make (struct type t = vertex let compare = compare end)

let vset_ids v = List.map vertex_id (VSet.elements v)

(* A signature matched by queues and stacks *)
module type BagType =
  sig
    type 'a t
    exception Empty
    val create : unit -> 'a t
    val push : 'a -> 'a t -> unit
    val pop : 'a t -> 'a (* raises Empty *)
    val top : 'a t -> 'a (* raises Empty *)
    val clear : 'a t -> unit
    val copy : 'a t -> 'a t
    val is_empty : 'a t -> bool
    val length : 'a t -> int
    val iter : ('a -> unit) -> 'a t -> unit
  end

(* A Bag is something that you can put things into *)
(* and take them out of *)

(* Functor that makes a Bag out of a queue or stack *)
module MakeBag =
  functor (X : BagType) ->
    struct
      exception Empty = X.Empty
      type 'a t = 'a X.t
      let create = X.create
      let put = X.push
      let get = X.pop (* raises Empty *)
      let next = X.top (* raises Empty *)
      let clear = X.clear
      let copy = X.copy
      let is_empty = X.is_empty
      let size = X.length
      let iter = X.iter
    end

(* use Queue for BFS, Stack for DFS *)
module Bag = MakeBag (Queue)

(* Simple graph traversal (BFS or DFS) returns a set of nodes
   accessible from a starting node. If dir is non-negative then follow
   outgoing edges, if negative then follow reversed incoming edges (i.e., paths
   in the graph with edges reversed. *)

let traverse v0 dir =
  let disc = Bag.create()
  and visited = ref VSet.empty in
    (* Expand the visited set to contain everything v goes to,
     * and add newly seen vertices to the stack/queue. *)
  let expand v =
    let handle_edge e =
      let (v, v', _) = edge_info e in
  if not (VSet.mem v' !visited)
  then (visited := (VSet.add v' !visited); Bag.put v' disc)
  else () in
  List.map handle_edge (if dir < 0 then (incoming_rev v) else (outgoing v))
  in
    (visited := VSet.add v0 !visited;
     Bag.put v0 disc;
     while (not (Bag.is_empty disc)) do
       ignore (expand (Bag.get disc))
     done;
     !visited)

(* Weakly connected component containing v0, simply the set of nodes
   accessible from this node.  *)

let component v0 = traverse v0 1

(* Strongly connected component containing v0, intersecting the weak
   components found using traverse in the graph and in the graph with
   the edges reversed.  *)

let strong_component v0 =
  VSet.inter (traverse v0 1) (traverse v0 (-1))

(* The strongly connected components form a partition of the vertices
 * (analogous to connected components of an undirected graph). This
 * partition can be computed in O(|V|+|E|) time, for instance with
 * Tarjan's algorithm, but here is a simple method that is a log factor
 * slower using OCaml sets. *)

let strong_components g =
  let vs = ref VSet.empty
  and cs = ref [] in
    (List.iter (fun v -> vs := VSet.add v !vs) (vertices g);
     while (not (VSet.is_empty !vs)) do
       let c = strong_component (VSet.choose !vs) in
       (vs := VSet.diff !vs c;
       cs := c::!cs)
     done;
     !cs)

(* Topological ordering for a DAG.  If the graph is cyclic then raise
 * an exception, otherwise choose a node with no incoming edges as the
 * first node in the ordering and append it to the result of
 * recursively considering the ordering of the remaining nodes.  The
 * running time of this version is O(|V|^2), whereas the asymptotically
 * fastest methods are O(|V|+|E|). *)

let topological_rec g =
  let rec topological_destr gr =
    let vl = vertices gr in
    if vl = [] then [] else
    let sl = List.filter (fun v -> in_degree v = 0) vl in
      if sl = [] (* No vertices without incoming edges, have a cycle *)
    then failwith "Graph is cyclic"
    else
      let v = List.hd sl in
      (remove_vertex gr v;
       v :: topological_destr gr) in
      topological_destr (copy g)

(* Iterative version of topological order, with O(|V|+|E|} running
   time. Note that while remove_vertex is O(|E|) time for a single
   vertex, it is also O(|E|) time when every vertex of the graph is
   removed because each edge is considered a constant number of times
   overall in the process of removing all the vertices. *)

let topological_iter g =
  let gr = copy g in
  let sl = ref (List.filter (fun v -> in_degree v = 0) (vertices gr))
  and revorder = ref [] in
    while !sl <> [] do
      let v = List.hd !sl in
      (sl := List.tl !sl;
      List.iter
     (function e ->
        match edge_info e with (_, dst, _) ->
          if in_degree dst = 1
          then sl := dst :: !sl else ())
        (outgoing v);
      remove_vertex gr v;
      revorder := v :: !revorder)
    done;
    if num_vertices gr = 0
    then List.rev !revorder
    (* Remaining vertices all with incoming edges, graph is cyclic *)
    else failwith "Graph is cyclic"


(* Simple test cases  *)

let g1 = create();;
let v11 = add_vertex g1;;
let v12 = add_vertex g1;;
let v13 = add_vertex g1;;
let v14 = add_vertex g1;;
add_edge (v11, v12, 1);;
add_edge (v12, v13, 1);;
add_edge (v13, v11, 1);;
add_edge (v13, v14, 1);;

vset_ids (component v11);;
vset_ids (component v12);;
vset_ids (component v13);;
vset_ids (component v14);;

vset_ids (strong_component v11);;
vset_ids (strong_component v12);;
vset_ids (strong_component v13);;
vset_ids (strong_component v14);;

List.map vset_ids (strong_components g1);;

let g2 = create();;
let v21 = add_vertex g2;;
let v22 = add_vertex g2;;
let v23 = add_vertex g2;;
let v24 = add_vertex g2;;
let v25 = add_vertex g2;;
let v26 = add_vertex g2;;
let v27 = add_vertex g2;;
add_edge (v21, v24, 1);;
add_edge (v21, v25, 1);;
add_edge (v21, v27, 1);;
add_edge (v22, v23, 1);;
add_edge (v22, v25, 1);;
add_edge (v22, v26, 1);;
add_edge (v23, v24, 1);;
add_edge (v23, v25, 1);;
add_edge (v24, v25, 1);;
add_edge (v25, v26, 1);;
add_edge (v25, v27, 1);;
add_edge (v26, v27, 1);;

List.map vertex_id (topological_rec g2);;
List.map vertex_id (topological_iter g2);;

List.map vertex_id (topological_rec g1);;
List.map vertex_id (topological_iter g1);;

(* How about using strong components to determine if a graph is acyclic? *)
