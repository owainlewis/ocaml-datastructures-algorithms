module Dijkstra = struct
  type cost = Nan | Cost of float
  
  type adj_mat = cost array array

  type 'a graph = { mutable ind : int;
                    size : int;
                    nodes : 'a array;
                    m : adj_mat }
   let create_graph n s =
     { ind = 0; size = s; nodes = Array.make s n;
       m = Array.make_matrix s s Nan }    
  let belongs_to n g =
     let rec aux i =
       (i < g.size) && ((g.nodes.(i) = n) || (aux (i+1)))
     in aux 0
  let add_node n g =
     if g.ind = g.size then failwith "Graph full"
     else if belongs_to n g then failwith "Node exists"
     else (g.nodes.(g.ind) <- n; g.ind <- g.ind + 1)
  let index n g =
     let rec aux i =
       if i >= g.size then raise Not_found
       else if g.nodes.(i) = n then i
            else aux (i+1)
     in aux 0
  let add_edge e1 e2 c g =
     try
       let x = index e1 g and y = index e2 g in
         g.m.(x).(y) <- Cost c
     with Not_found -> failwith "node does not exist"
  let gr = create_graph 0 10;;
  let test_graph () =
     let g = create_graph "nothing" 5 in
       List.iter (fun x -> add_node x g) ["A"; "B"; "C"; "D"; "E"];
       List.iter (fun (a,b,c) -> add_edge a b c g)
         ["A", "B", 10.;
          "A", "D", 30.;
          "A", "E", 100.0;
          "B", "C", 50.;
          "C", "E", 10.;
          "D", "C", 20.;
          "D", "E", 60.];
       for i=0 to g.ind -1 do g.m.(i).(i) <- Cost 0.0 done;
        g;;
end
