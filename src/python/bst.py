# Binary Search Tree

class BinarySearchTree(object):

    def __init__(self, value=None):
	self.left = None
	self.right = None
	self.value = value

    def insert(self, value):
        """
        Insert a value into the tree
        """ 
        # If value is empty
        if self.value is None:
            self.value = value
        # Go left
	elif self.value > value:
	   if self.left is None:
	      self.left = BinarySearchTree(value)
	   else:
	      self.left.insert(value)
        # Go right
	else:
	   if self.right is None:
	      self.right = BinarySearchTree(value)
	   else:
	      self.right.insert(value)

    def search(self, value, parent=None):
        """
            Search for a value in the tree
        """
        if value < self.value:
            if self.left is None:
                return False
            return self.left.search(value, self)
        elif value > self.value:
            if self.right is None:
                return False
            return self.right.search(value, self)
        else:
            return True # value found

    def delete(self, node): pass
            
    def min_value(self, node): pass
 
    def max_value(self, node): pass

if __name__ == '__main__':
    b = BinarySearchTree()
    b.insert(3)
    b.insert(6)
    b.insert(5)
    b.insert(2)
   
