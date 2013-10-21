(*************************

  Graph algorithms

  Owain Lewis 2013

******************************)

module type GRAPH =
  sig
    type 'a vertex
    type graph
    val add_vertex : graph -> 'a vertex -> graph
    val add_edge   : graph -> 'a vertex -> 'a vertex -> graph
  end

module UndirectedGraph : GRAPH =
  struct
    exception EmptyGraph

    type 'a vertex = 'a
    type graph = (int * int) list

    let add_vertex g vertex = g

    let add_edge g v1 v2 = g
end
