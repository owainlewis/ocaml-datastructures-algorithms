# Adjacency list rep

# G is a directed graph

G = {'A': ['C', 'D'], 
     'B': ['D', 'A'], 
     'C': ['D', 'E'],
     'D': ['E'], 
     'E': [] }

# Python implementation of the graph G above

class AdjacencyGraph(object):

    """ A class that represents a graph as an adjacency list """

    def __init__(self):
        self.node = {}
        self.edges = {}

    def add_node(self, node, **attrs):
        if node not in self.edges:
            self.edges[node] = {}
            self.node[node] = attrs
        else:
            self.node[node].update(attrs)

    def add_edge(self, u, v, **attrs): pass

def make_graph():
    gr = AdjacencyGraph()
    items = ['A', 'B', 'C', 'D', 'E']
    for v in items:
        gr.add_node(v)
    return gr

gr = make_graph()

class AdjList:

    def __init__(self, g):
        self.g = g
   
    def vertices(self):
        return self.g.keys()

    def adjacent(self, v):
        return self.g[v]

g = AdjList(G)

print(g.vertices())
print(g.adjacent('C'))
