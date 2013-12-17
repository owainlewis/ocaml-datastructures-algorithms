class Edge(object):
    """ A directed weighted graph edge """
    def __init__(self, start, end, cost):
        self.start = start
        self.end   = end
        self.cost  = cost

    def __repr__(self):
        return '%s - (%s) -> %s' % ( self.start, self.cost, self.end )

class Graph():
    """ A directed weighted graph """
    def __init__(self, edges):
        self.edges = [Edge(*edge) for edge in edges]
        self.vertices = set(sum(([e.start, e.end] for e in self.edges), []))

graph = Graph([("a", "b", 7),  ("a", "c", 9),  ("a", "f", 14), ("b", "c", 10),
               ("b", "d", 15), ("c", "d", 11), ("c", "f", 2),  ("d", "e", 6),
               ("e", "f", 9)])
