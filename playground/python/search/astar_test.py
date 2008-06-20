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
    def possible_moves(self,state):
        return ((1,"up"),(1,"down"),(1,"left"),(1,"right"))
    def make_move(self,state,move):
        (x,y) = state
        if move == "left":
            return (x-3,y)
        if move == "right":
            return (x+5,y)
        if move == "up":
            return (x,y+3)
        if move == "down":
            return (x,y-2)
        raise "Invalid move"

def test():
    sol = astar.path_to_list(astar.astar(TestProblem()).next())
    print len(sol)
    print sol

test()
