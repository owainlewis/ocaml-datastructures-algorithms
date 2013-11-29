# Python binary heap refresher

class BinaryHeap(object):

    def __init__(self, heap=[0]):
        # to make the maths easier array is indexed from 1 not 0
        self.heap         = heap
        self.current_size = 0
    
    def __repr__(self): return str(self.heap)
    
    def increment_size(self):
        self.current_size += 1

    def get_last_inserted_item(self): return self.current_size

    def parent_index(self, node_index):
        """Parent node can always be found with integer division"""
        return node_index // 2

    def left_child_index(self, node_index): 
        """Returns the index of the left child node for a given node"""
        return i * 2

    def right_child_index(self, node_index): 
        """Returns the index of the left child node for a given node"""
        return (i * 2) + 1

    def percolate_up_heap(self,i, binary_heap):
        """Swims an item up the heap to it's correct position so that it does not violate
           heap invariant"""
        while self.parent_index(i) > 0:
            parent_index      = self.parent_index(i)
            node_value        = binary_heap[i]
            parent_node_value = binary_heap[parent_index]
            # if the current node is g
            if node_value < parent_node_value:
                # swap parent for current node value
                binary_heap[parent_index] = node_value
                # swap current node value with previous parent value
                binary_heap[i] = parent_node_value
            i = parent_index
        return binary_heap

    def insert(self, value):
        self.heap.append(value)
        self.increment_size()
        self.heap = self.percolate_up_heap(self.current_size, self.heap)
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
        
test = [0,6,7,12,10,15,17,5]
