heap = [16, 14, 10, 8, 7, 9, 3, 2, 4, 1]

class Heap(object):

    def __init__(self, heap):
        self.heap = heap

    # The n values are not zero indexed
    def getHeap(self, n):
        return self.heap[n-1]

    # Find the parent of element at index n
    def parent(self, n):
        idx = n/2
        return self.getHeap(idx)

    def leftChild(self, n):
        idx = 2 * n
        return self.getHeap(idx)

    def rightChild(n):
        idx = (2 * n) + 1
        return getHeap(idx)

    def __str__(self):
        for x in self.heap:
            print x

