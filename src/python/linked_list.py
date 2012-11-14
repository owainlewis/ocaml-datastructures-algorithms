# Single linked list more or less just straight from my Java version

class Node(object):

    def __init__(self, data=None, nextNode=None):
        self.data = data;
        self.next = nextNode;

class LinkedList():

    """ List ADT """

    def __init__(self):
        self.head = None
        self.tail = None

    def _add(self, node, data):
        """ Auxilary function that inserts into a specific node """
        new_node = Node(data, node.next)
        node.next = new_node
        if self.tail == node:
            self.tail = new_node

    def add(self, data):
        """ Add an item to the tail """
        if self.tail is None:
            new_node = Node(data, None)
            self.head = self.tail = new_node
        else:
            self._add(self.tail, data)

    def get(self, index): pass

if __name__ == '__main__':

  l = LinkedList()

  l.add(2)

  print l.head.next
        

        
