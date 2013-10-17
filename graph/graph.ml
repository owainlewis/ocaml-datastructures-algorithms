(*************************

  Graph algorithms

  Owain Lewis 2013

******************************)

module type GRAPH =
  sig
    type 'a vertex
    type graph
    val empty : graph
    val is_empty : graph -> bool
    val add_vertex : graph -> 'a vertex -> graph
  end

module UndirectedGraph : GRAPH =
  struct
    type 'a vertex = 'a
    type graph = (int * int) list
    exception EmptyGraph
    let empty : graph = []

    let is_empty (g: graph) : bool =
      match g with
        | [] -> true
        | _ -> false

    let add_vertex(g: graph) (v: int vertex) = [(v, [])] :: g
end
