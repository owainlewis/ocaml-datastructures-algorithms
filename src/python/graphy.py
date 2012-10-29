#! /usr/bin/env python

# Directed Graph Algorithms 

class Vertex:

    """ A class that represents a graph vertex """

    def __init__(self, key):
        self.id = key
        self.connectedTo = {}

    def addNeighbour(self, neighbour, weight=0):
        self.connectedTo[neighbour] = weight

    def __str__(self):
        connections = str([x.id for x in self.connectedTo])
        return str(self.id) + ' connectedTo: ' + connections

    def getConnections(self):
        return self.connectedTo.keys()

    def getId(self):
        return self.id

    def getWeight(self,neighbour):
        return self.connectedTo[neighbour]

class Graph:

    def __init__(self):
        self.vertexList = {}
        self.numVertices = 0

    def addVertex(self,key):
        self.numVertices = self.numVertices + 1
        newVertex = Vertex(key)
        self.vertexList[key] = newVertex
        return newVertex

    def getVertex(self,n):
        if self.member(n):
            return self.vertexList[n]
        else:
            return None

    def member(self,n):
        return n in self.vertexList

    def addEdge(self,f,t,cost=0):
        if f not in self.vertexList:
            nv = self.addVertex(f)
        if t not in self.vertexList:
            nv = self.addVertex(t)
        self.vertexList[f].addNeighbour(self.vertexList[t], cost)

    def getVertices(self):
        return self.vertexList.keys()

    def __iter__(self):
        return iter(self.vertexList.values())

def main():
    g = Graph()
    g.addVertex(1)
    g.addVertex(2)
    g.addEdge(1,2,3)
    print(g.getVertices)

if __name__ == '__main__':
    main()
