# be aware of python's strange scoping rules...
# they made me lose an hour hunting for a
# subtle but simple bug -- magnus

def test():
    while True:
        a = 5
        break
    # a is not gone!
    # ...in a saner language a should not exist anymore.
    print a

    for i in [1,2]:
        if i == 2:
            print b # b from last iteration still exists!
                    # ...in a saner language b should not exist anymore.
        b = i

test()


# another funny thing:

def boo(x):
    def baa():
        print x   # this is okay
    baa()

boo(1)

def moo():
    x = 1
    def maa():
        x = x     # this is not okay
    maa()


moo()
