(* Union-Find (Disjoint Set) Implementation *)

(* A Union-Find data structure (also called Disjoint Set) efficiently
   keeps track of a set of elements partitioned into disjoint subsets,
   supporting two operations:
   - Find: Determine which set an element belongs to
   - Union: Join two sets together
*)

type t = {
  parent: int array;  (* parent[i] = parent of i *)
  rank: int array;    (* rank[i] = rank of the tree rooted at i *)
  mutable sets: int;  (* Number of disjoint sets *)
}

(* Create a new Union-Find data structure with n elements *)
let create n =
  let parent = Array.init n (fun i -> i) in  (* Each element is its own parent *)
  let rank = Array.make n 0 in  (* All ranks start at 0 *)
  { parent; rank; sets = n }

(* Find the representative of the set containing x, with path compression *)
let rec find uf x =
  if uf.parent.(x) <> x then
    uf.parent.(x) <- find uf uf.parent.(x);  (* Path compression *)
  uf.parent.(x)

(* Union the sets containing x and y, using rank heuristic *)
let union uf x y =
  let root_x = find uf x in
  let root_y = find uf y in
  
  if root_x = root_y then
    false  (* x and y are already in the same set *)
  else begin
    (* Union by rank: attach smaller rank tree under root of higher rank tree *)
    if uf.rank.(root_x) < uf.rank.(root_y) then
      uf.parent.(root_x) <- root_y
    else if uf.rank.(root_x) > uf.rank.(root_y) then
      uf.parent.(root_y) <- root_x
    else begin
      (* Same rank, make one the parent and increment its rank *)
      uf.parent.(root_y) <- root_x;
      uf.rank.(root_x) <- uf.rank.(root_x) + 1
    end;
    
    uf.sets <- uf.sets - 1;
    true  (* Union was successful *)
  end

(* Check if two elements are in the same set *)
let connected uf x y =
  find uf x = find uf y

(* Get the number of disjoint sets *)
let count uf = uf.sets

(* Get all elements in the same set as x *)
let get_set uf x =
  let root_x = find uf x in
  let result = ref [] in
  for i = 0 to Array.length uf.parent - 1 do
    if find uf i = root_x then
      result := i :: !result
  done;
  List.rev !result

(* Get all the sets as a list of lists *)
let get_all_sets uf =
  let n = Array.length uf.parent in
  let sets = Hashtbl.create n in
  
  (* Group elements by their representative *)
  for i = 0 to n - 1 do
    let root = find uf i in
    match Hashtbl.find_opt sets root with
    | None -> Hashtbl.add sets root [i]
    | Some elements -> Hashtbl.replace sets root (i :: elements)
  done;
  
  (* Convert the hashtable to a list of lists *)
  Hashtbl.fold (fun _ elements acc -> elements :: acc) sets []

(* Applications of Union-Find *)

(* Detect cycle in an undirected graph *)
let has_cycle edges n =
  let uf = create n in
  let rec check = function
    | [] -> false
    | (u, v) :: rest ->
        if connected uf u v then
          true  (* Cycle found *)
        else begin
          ignore (union uf u v);
          check rest
        end
  in
  check edges

(* Kruskal's algorithm for Minimum Spanning Tree *)
let kruskal edges n =
  (* Sort edges by weight *)
  let sorted_edges = List.sort (fun (_, _, w1) (_, _, w2) -> compare w1 w2) edges in
  
  let uf = create n in
  let mst = ref [] in
  
  List.iter (fun (u, v, weight) ->
    if not (connected uf u v) then begin
      ignore (union uf u v);
      mst := (u, v, weight) :: !mst
    end
  ) sorted_edges;
  
  List.rev !mst

(* Find the number of connected components in a graph *)
let connected_components edges n =
  let uf = create n in
  
  List.iter (fun (u, v) ->
    ignore (union uf u v)
  ) edges;
  
  count uf