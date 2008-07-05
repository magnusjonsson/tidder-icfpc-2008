import rbtree
from time import clock

def dump(t, i = 0):
    if t:
        print "  " * i + ("Red" if t.is_red else "") + ("Black" if t.is_black else "") + " " + str(t.key) + ": " + str(t.value)
        dump(t.left, i + 1)
        dump(t.right, i + 1)

def main():
    ops = 10000
    
    t1 = clock()
    
    foo = {}
    for i in range(0, ops):
        foo[i] = i
    for i in range(0, ops):
        foo[i] = foo[i] + i
    for i in range(0, ops):
        assert foo[i] == i + i
    
    t2 = clock()
    
    bar = rbtree.empty
    for i in range(1, ops):
        bar = bar.with_(i, i)
    for i in range(1, ops):
        bar = bar.with_(i, bar[i] + i)
    for i in range(1, ops):
        assert bar[i] == i + i
    t3 = clock()
    
    print "dict:   " + str(t2 - t1)
    print "rbtree: " + str(t3 - t1)
    
    #dump(bar)

if __name__ == "__main__":
    main()
