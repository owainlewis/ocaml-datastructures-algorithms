# AVL Trees

class BSTNode(object):
    def __init__(self, t):
        self.key    = t
        self.dissoc()

    def dissoc(self):
        self.left = None
        self.right = None
        self.parent = None

    def __repr__(self): 
        return "Node: < %s >" % self.key

# Binary search tree
############################################

class BST(object):

    def __init__(self):
        self.root = None

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
   
# AVL Tree
##########################################

class AVLTree(BST):

    def __init__(self):
        pass



