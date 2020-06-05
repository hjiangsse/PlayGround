class IntTrie:
    def __init__(self):
        self.left = self.right = None
        self.value = None

def insert(t, key, value = None):
    if t is None:
        t = IntTrie()
    p = t
    while key != 0:
        if key & 1 == 0:
            #insert to the left tree
            if p.left is None:
                p.left = IntTrie()
            p = p.left
        else:
            if p.right is None:
                p.right = IntTrie()
            p = p.right
        key = key >> 1 # key / 2
    p.value = value
    return t
