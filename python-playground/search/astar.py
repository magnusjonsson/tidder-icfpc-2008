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
    hcache = {}
    heap = []
    def addState(state,depth,path):
        previous_depth = best_depth.get(state)
        if previous_depth == None or depth < previous_depth:
            best_depth[state] = depth
            h = hcache.get(state)
            if h == None:
                h = problem.heuristic(state)
                hcache[state] = h
            heapq.heappush(heap, _QueueItem(depth+h, depth, state, path))
    addState(problem.initial_state(), 0, ())
    while heap:
        qitem = heapq.heappop(heap)
        state = qitem.state
        depth = qitem.depth
        path = qitem.path
        if problem.is_goal(state):
            yield path
        del hcache[state]  # save some memory from now on
        for (cost,move) in problem.possible_moves(state):
            next_state = problem.make_move(state,move)
            addState(next_state, depth+cost, (move, path))

def path_to_list(path):
    list = []
    while path:
        head,path = path
        list.append(head)
    return list

    
