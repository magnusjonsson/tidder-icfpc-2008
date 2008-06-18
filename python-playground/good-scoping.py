def hello(x):
    def hi(x):
        print x
    print x
    hi(x+1)
    print x

hello(1)
