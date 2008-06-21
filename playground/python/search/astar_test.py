import astar

class TestProblem:
    target = (253,252)
    def initial_state(self):
        return (0,0)
    def is_goal(self,state):
        return state == self.target
    def heuristic(self,state):
        (x,y) = state
        (tx,ty) = self.target 
        return (abs(x-tx)+4)/5 + (abs(y-ty)+2)/3
    def generate_moves(self,state):
        (x,y) = state
        yield (1,"up",(x,y+3))
        yield (1,"down",(x,y-2))
        yield (1,"left",(x-3,y))
        yield (1,"right",(x+5,y))

def test():
    sol = astar.path_to_list(astar.astar(TestProblem()).next())
    print len(sol)
    print sol

test()
