# Adjacency list rep

# G is a directed graph

G = {'A': ['C', 'D'], 
     'B': ['D', 'A'], 
     'C': ['D', 'E'],
     'D': ['E'], 
     'E': [] }

class AdjList:

    def __init__(self, g):
        self.g = g

    def __str__(self): pass
   
    def vertices(self):
        return self.g.keys()

    def adjacent(self, v):
        return self.g[v]

g = AdjList(G)

print(g.vertices())
print(g.adjacent('C'))
