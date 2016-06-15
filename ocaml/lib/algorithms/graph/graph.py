# An undirected weighted graph
#
class UndirectedGraph(object):
    """ A simple undirected graph representation
        for shortest path problems """
    def __init__(self):
        self.vertices = set()
        self.edges = {}
        self.weights = {}

    def add_vertex(self, value):
        """ Add a vertex to the graph """
        self.vertices.add(value)

    def add_edge(self, from_node, to_node, weight):
        """ Since this graph is undirected connections go both ways """
        self._add_edge(from_node, to_node, weight)
        self._add_edge(to_node, from_node, weight)

    def _add_edge(self, from_node, to_node, weight):
        """ Add a weighted edge between two vertices """
        self.edges.setdefault(from_node, set())
        self.edges[from_node].add(to_node)
        self.weights[(from_node, to_node)] = weight

    def weight(self, from_node, to_node):
        """ Returns the weight between two vertices """
        return self.weights[(from_node, to_node)]

def main():
    g = UndirectedGraph()
    g.nodes = set(range(1, 7))
    g.add_edge(1, 2, 7)
    g.add_edge(1, 3, 9)
    return g
