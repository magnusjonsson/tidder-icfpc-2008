# Each game state has a heuristic score associated with it.
# Ending states has an end score associated with them.

# End scores and heuristic scores must be compatible, ie it should
# be possible to compare them and get a meaningful result.

# There are two types of player:
# 'max' : will try to maximize the score
# 'min' : will try to minimize the score
#
# Every player will play optimally.

def make(endScore,          # state -> None | score
         heuristicScore,    # state -> score
         currentPlayerGoal, # state -> "max" | "min"
         generateMoves      # state -> (cost,move,nextState) sequence
         ):
    cache = {}

    def search(state, depth, lowerBound, upperBound):
        lb = lowerBound
        ub = upperBound

        if state in cache:
            (cachedDepth,cachedLB,cachedUB,cachedScore) = cache[state]
            if depth <= cachedDepth and cachedLB <= lb and cachedUB >= ub:
                return cachedScore

        es = endScore(state)
        if es != None:
            return es
        
        if depth <= 0:
            h = heuristicScore(state)
            return h

        goal = currentPlayerGoal(state)
        
        # TODO: consider moves in order of most promising to
        # least promising order to facilitate early pruning
        # of the search tree.

        for (cost,move,nextState) in generateMoves(state):
            s = search(nextState,depth-cost,lowerBound,upperBound)
            if goal == 'max':
                if s > lowerBound:
                    lowerBound = s
                    if s > upperBound:
                        break
            elif goal == 'min':
                if s < upperBound:
                    upperBound = s
                    if s < lowerBound:
                        break
            else:
                raise ('Strange goal:'+str(goal))

            
        if goal == 'max':
            score = lowerBound
        elif goal == 'min':
            score = upperBound

        cache[state] = (depth,lb,ub,score)

        return score

            
    def optimalMove(state,depth):
        goal = currentPlayerGoal(state)
        lowerBound = -10001
        upperBound =  10001
        bestMove = None
        bestNextState = None
        # todo: consider moves from most promising to least promising
        for (cost,move,nextState) in generateMoves(state):
            s = search(nextState,depth-cost,lowerBound,upperBound)
            if goal == 'max':
                if s > lowerBound:
                    lowerBound = s
                    bestMove = move
                    bestNextState = nextState
            elif goal == 'min':
                if s < upperBound:
                    upperBound = s
                    bestMove = move
                    bestNextState = nextState
            else:
                raise ('Strange goal: '+str(goal))
        return (bestMove,bestNextState)

    return optimalMove
