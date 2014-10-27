# Python DSA

from __future__ import print_function

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

class BSTNode(object):
    """ A simple BST node object """
    def __init__(self, t):
        self.key    = t
        self.dissoc()

    def dissoc(self):
        self.left = None
        self.right = None
        self.parent = None

    def is_leaf(self):
        return self.key is None

    def __repr__(self): 
        return "Node: < %s >" % self.key

# ===================================================
#
# Binary search tree
#
# ===================================================

class BST(object):
    """ A basic Binary Search Tree """
    def __init__(self, values=[]):
        self.root = None
        if any(values):
            self.insert_many(values)

    def insert(self, k):
        """Insert a key k into the tree"""
        new = BSTNode(k)
        if self.root is None:
            self.root = new
        else:
            node = self.root
            while True:
                # Go left
                if k < node.key:
                    if node.left is None: # if left is none insert it here
                        node.left = new
                        new.parent = node
                        break
                    node = node.left # carry on down
                else:
                     if node.right is None:
                         node.right = new
                         new.parent = node
                         break
                     node = node.right
        return new

    def insert_many(self, values=[]):
        return [self.insert(value) for value in values]
         
    def find(self, v):
        node = self.root
        while node is not None:
            if v == node.key:
                return node
            elif v < node.key:
                node = node.left
            else: node = node.right
        return None

    def contains(self, v): 
        return self.find(v) is not None

    def delete_min(self):
        if self.root is None:
            return None
        else: # smallest item will be in the left sub tree so walk it
            node = self.root
            while node.left is not None:
                node = node.left
            if node.parent is not None:
                node.parent.left = node.right
            else: self.root = node.right
            if node.right is not None:
                node.right.parent = node.parent
            parent = node.parent
            node.dissoc()
            return node, parent

    def height(self, node):
        if node is None:
            return 0
        else:
            return 1 + max(self.height(node.left), self.height(node.right))

    # Traversals 
    def print_func (self, x): 
        print(x) # wrong version of python to pass as a lambda

    def inorder_traversal(self, f, node):
        if node is not None:          
            self.inorder_traversal(f, node.left)
            f(node.key)
            self.inorder_traversal(f, node.right)

    def print_tree_in_order(self):
        self.inorder_traversal(self.print_func, self.root)

    def preorder_traversal(self, f, node):
        if node is not None:
            f(node.key)
            self.preorder_traversal(f, node.left)
            self.preorder_traversal(f, node.right)

    def print_tree_in_preorder(self):
        self.preorder_traversal(self.print_func, self.root)

    def postorder_traversal(self, f, node):
        if node is not None:
            self.postorder_traversal(f, node.left)
            self.postorder_traversal(f, node.right)
            f(node.key)

    def print_tree_in_postorder(self):
        self.postorder_traversal(self.print_func, self.root)
        
# AVL Tree
##########################################

class AVLTree(BST):
    def __init__(self):
        pass
