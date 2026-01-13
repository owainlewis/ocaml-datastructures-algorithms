(* Structures Module - Entry point for data structures *)

module Graph = struct
  exception Vertex_not_found = Graph.Vertex_not_found
  exception Cyclic = Graph.Cyclic

  module DiGraph = Graph.DiGraph

  let topological_sort = Graph.topological_sort
  let dfs_edges = Graph.dfs_edges
end

module Queue = struct
  exception Empty_queue = Queue.Empty_queue

  type 'a t = 'a Queue.t

  let empty = Queue.empty
  let is_empty = Queue.is_empty
  let enqueue = Queue.enqueue
  let dequeue = Queue.dequeue
  let front = Queue.front
  let peek = Queue.peek
  let of_list = Queue.of_list
  let to_list = Queue.to_list
  let length = Queue.length
end

module PriorityQueue = struct
  exception Empty_queue = Priority_queue.Empty_queue

  type 'a t = 'a Priority_queue.t

  let empty = Priority_queue.empty
  let is_empty = Priority_queue.is_empty
  let insert = Priority_queue.insert
  let extract = Priority_queue.extract
  let peek = Priority_queue.peek
  let of_list = Priority_queue.of_list
end

module Search = struct
  let linear_search = Search.linear_search
  let binary_search = Search.binary_search
  let binary_search_list = Search.binary_search_list
  let find_first = Search.find_first
  let find_all = Search.find_all
end

module Distance = struct
  type point = Distance.point

  let euclidean = Distance.euclidean
  let manhattan = Distance.manhattan
  let chebyshev = Distance.chebyshev
  let minkowski = Distance.minkowski
  let euclidean_nd = Distance.euclidean_nd
  let manhattan_nd = Distance.manhattan_nd
  let hamming = Distance.hamming
end

module Shuffle = struct
  let fisher_yates = Shuffle.fisher_yates
  let shuffle_list = Shuffle.shuffle_list
  let random_permutation = Shuffle.random_permutation
  let sample = Shuffle.sample
  let sample_list = Shuffle.sample_list
end
