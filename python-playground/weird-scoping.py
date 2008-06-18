# be aware of python's strange scoping rules...
# they made me lose hours bughunting -- magnus

def test():
    while True:
        a = 5
        break
    # a is not gone!
    # in a sane language a should not exist anymore.
    print a

    for i in range(1,2):
        if i == 2:
            print b # b from last iteration still exists!
                    # in a sane language it should not exist anymore.
        b = i

test()
