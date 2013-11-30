# AVL Trees

from __future__ import print_function

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

# Binary search tree
############################################

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

    # This is easier in OCaml
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
        
test = [3, 5, 6, 7, 10, 12, 13, 15, 16, 18, 20, 23]

# AVL Tree
##########################################

class AVLTree(BST):

    def __init__(self):
        pass



