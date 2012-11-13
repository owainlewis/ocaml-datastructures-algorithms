import unittest

# Bag ADT using a list

class BagADT(object):

    def __init__(self):
        self._items = []
    
    def __len__(self):
        """ The number of items in the bag """
        return len(self._items)

    def contains(self, item):
        """ Check if the bag contains and item """
        return item in self._items

    def add(self, item):
        """ Add an item to the bag """
        self._items.append(item)

    def remove(self, item):
        """ Delete an item from the bag """
        assert self.contains(item)
        return self._items.pop(self._items.index(item))


class TestBagADT(unittest.TestCase):

    def setUp(self):
        self.b = BagADT()
        [self.b.add(x) for x in range(0,10)]
    
    def test_length(self):
        self.assertEqual(len(self.b), 10)

    def test_contains(self):
        self.assertEqual(self.b.contains(1), True)

    def test_deletion(self):
        self.assertEqual(self.b.contains(1), True)
        self.b.remove(1)
        self.assertEqual(self.b.contains(1), False)

if __name__ == '__main__':
    unittest.main()
