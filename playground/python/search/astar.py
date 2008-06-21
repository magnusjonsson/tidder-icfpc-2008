import heapq
import math

class _QueueItem:
    def __init__(self,priority,depth,state,path):
        self.priority = priority
        self.depth = depth
        self.state = state
        self.path = path
    def __cmp__(self,other):
        return cmp(self.priority, other.priority)

def astar(problem):
    best_depth = {}
    heap = []
    def addState(state,depth,path):
        previous_depth = best_depth.get(state)
        if previous_depth == None or depth < previous_depth:
            best_depth[state] = depth
            h = problem.heuristic(state)
            heapq.heappush(heap, _QueueItem(depth+h, depth, state, path))
    addState(problem.initial_state(), 0, ())
    while heap:
        qitem = heapq.heappop(heap)
        state = qitem.state
        depth = qitem.depth
        path = qitem.path
        if problem.is_goal(state):
            yield path
        for (cost,move,next_state) in problem.generate_moves(state):
            addState(next_state, depth+cost, (move, path))

def path_to_list(path):
    list = []
    while path:
        head,path = path
        list.append(head)
    return list  
