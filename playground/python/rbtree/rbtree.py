# fh: Red-Black Trees in Python.

# These have a functional interface, by which I mean that all "mutation"
# methods return a modified tree and leave the original tree alone. What this
# means is that rbtrees are *fully persistent*, 

# This is more or less a port of
# http://www.cs.kent.ac.uk/people/staff/smk/redblack/Untyped.hs
# although both the interface and the implementation have been modified to
# be more idiomatic Python.

# The only exported symbol is rbtree.empty. An rbtree t supports the following
# methods:
#     key in t    # returns True if t contains a mapping for key, else False
#     t[key]    # returns the value mapped to key in t
#     t.with_(key, value)   # returns a copy of t modified to map key to value
#     t.without(key)    # returns a copy of t without a mapping for key

# without is not yet implemented.
# for practical use, further helper methods like max, without_max, min,
# without_min, and a range queries would need to be defined.

class _Empty(object):
    def __init__(self):
        pass
    
    is_red = property(lambda self: False)
    is_black = property(lambda self: True)
    
    def __nonzero__(self):
        return False
    
    def __contains__(self, key):
        return False
    
    def __getitem__(self, key):
        return KeyError(key)
    
    def with_(self, key, value):
        return _BlackNode(key, value, self, self)
    
    def with_red(self, key, value):
        return _RedNode(key, value, self, self)
    
    def without(self, key, value):
        return self

empty = _Empty()

class _TreeNode(object):
    def __init__(self, key, value, left, right):
        self.__key = key
        self.__value = value
        self.__left = left
        self.__right = right
    
    # expose these as properties to make sure they are immutable
    key = property(lambda self: self.__key)
    value = property(lambda self: self.__value)
    left = property(lambda self: self.__left)
    right = property(lambda self: self.__right)
    
    def __nonzero__(self):
        return True
    
    def __contains__(self, key):
        c = cmp(key, self.__key)
        if c < 0:
            return self.__left.__contains__(key)
        elif c > 0:
            return self.__right.__contains__(key)
        else:
            return value
    
    def __getitem__(self, key):
        c = cmp(key, self.__key)
        if c < 0:
            return self.__left.__getitem__(key)
        elif c > 0:
            return self.__right.__getitem__(key)
        else:
            return self.__value
    
    def with_(self, key, value):
        r = self.with_red(key, value)
        if r.is_red:
            return _BlackNode(r.__key, r.__value, r.__left, r.__right)
        else:
            return r

class _BlackNode(_TreeNode):
    def __init__(self, key, value, left = empty, right = empty):
        _TreeNode.__init__(self, key, value, left, right)
    
    is_red = property(lambda self: False)
    is_black = property(lambda self: True)
    
    def with_red(self, key, value):
        c = cmp(key, self.key)
        if c < 0:
            left = self.left.with_red(key, value)
            right = self.right
        elif c > 0:
            left = self.left
            right = self.right.with_red(key, value)
        else:   
            return _BlackNode(key, value, self.left, self.right)
        
        # rebalance if a child is red and has itself a red child
        
        if left.is_red:
            if left.left.is_red:
                left2 = _BlackNode(left.left.key, left.left.value, left.left.left, left.left.right)
                right2 = _BlackNode(self.key, self.value, left.right, right)
                return _RedNode(left.key, left.value, left2, right2)
            elif left.right.is_red:
                left2 = _BlackNode(left.key, left.value, left.left, left.left.left)
                right2 = _BlackNode(self.key, self.value, left.right.right, right)
                return _RedNode(left.right.key, left.right.value, left2, right2)
        elif right.is_red:
            if right.right.is_red:
                left2 = _BlackNode(self.key, self.value, left, right.left)
                right2 = _BlackNode(right.right.key, right.right.value, right.right.left, right.right.right)
                return _RedNode(right.key, right.value, left2, right2)
            elif right.left.is_red:
                left2 = _BlackNode(self.key, self.value, left, right.left.left)
                right2 = _BlackNode(right.key, right.value, right.left.right, right.right)
                return _RedNode(right.left.key, right.left.value, left2, right2)
        
        return _BlackNode(self.key, self.value, left, right)

class _RedNode(_TreeNode):
    def __init__(self, key, value, left = empty, right = empty):
        _TreeNode.__init__(self, key, value, left, right)

    is_red = property(lambda self: True)
    is_black = property(lambda self: False)

    def with_red(self, key, value):
        c = cmp(key, self.key)
        if c < 0:
            return _RedNode(self.key, self.value, self.left.with_red(key, value), self.right)
        elif c > 0:
            return _RedNode(self.key, self.value, self.left, self.right.with_red(key, value))
        else:
            return _RedNode(key, value, self.left, self.right)
