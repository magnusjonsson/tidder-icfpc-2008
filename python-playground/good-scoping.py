# in this case, python does the right thing.

def hello(x):
    def hi(x):
        print x
    print x
    hi(x+1)
    print x

hello(1)

print "---"

# here too.

def bye(x):
    def cu(y):
        x = y
    print x
    cu(x+1)
    print x

bye(1)
