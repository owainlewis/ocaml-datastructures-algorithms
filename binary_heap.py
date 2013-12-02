# ===================================================
#
# Python Binary Heap
#
# ===================================================

class BinaryHeap:

    def __init__(self):
        # to make the maths easier array is indexed from 1 not 0
        self.heap         = [0]
        self.current_size = 0

    def __repr__(self): return str(self.heap)

    def increment_size(self):
        self.current_size += 1

    def last_item(self):
        """ Returns the last item in the heap since current size
            is a reference to the largest index """
        return self.heap[self.current_size]

    def last_item_index(self): return self.current_size

    def parent_index(self, i):
        """Parent node can always be found with integer division"""
        return i // 2

    def left_child_index(self, i):
        """Returns the index of the left child node for a given node"""
        return i * 2

    def right_child_index(self, i):
        """Returns the index of the left child node for a given node"""
        return (i * 2) + 1

    # Swim UP the heap
    def swim(self,i):
        """Swims an item up the heap to it's correct position so that it does not violate
           heap invariant"""
        while self.parent_index(i) > 0:
            parent_index      = self.parent_index(i)
            node_value        = self.heap[i]
            parent_node_value = self.heap[parent_index]
            # if the current node is g
            if node_value < parent_node_value:
                # swap parent for current node value
                self.heap[parent_index] = node_value
                # swap current node value with previous parent value
                self.heap[i] = parent_node_value
            i = parent_index
        return self.heap

    def sink(self, i):
        while self.left_child_index(i) <= self.current_size:
            min_child = self.min_child(i)
            if self.heap[i] > self.heap[min_child]:
                # Swap
                cache = self.heap[i]
                self.heap[i] = self.heap[min_child]
                self.heap[min_child] = cache
            i = min_child

    def insert(self, value):
        self.heap.append(value)
        self.increment_size()
        self.heap = self.swim(self.last_item_index())
        return self.heap

    def min_child(self,i):
        left_child = self.left_child_index(i)
        right_child = self.right_child_index(i)
        if right_child > self.current_size:
            return left_child
        else:
            if self.heap[left_child] < self.heap[right_child]:
                return left_child
            else:
                return right_child

    def delete_min(self):
        """ Remove the smallest element from the heap """
        min_item = self.heap[1] # The smallest item will be the root
        self.heap[1] = self.last_item()
        self.sink(1)
        self.current_size = self.current_size - 1
        self.heap.pop()
        return self.heap

