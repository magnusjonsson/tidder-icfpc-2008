class Object(object):
    def __init__(self, kind, x, y, r):
        self.kind = kind
        self.x = x
        self.y = y
        self.r = r

class Martian(object):
    def __init__(self, x, y, dir, speed):
        self.x = x
        self.y = y
        self.dir = dir
        self.speed = speed
	
