# Binary Tree

class BTreeNode(object):

    def __init__(self, value, left=None, right=None):
        self.value = value
        self.left  = left
        self.right = right

    def __str__(self):
        return "%s" % self.value

class BinaryTree(object):

    def __init__(self, root=None):
        self.root = root

