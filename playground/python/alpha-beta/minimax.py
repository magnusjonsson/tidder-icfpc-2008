# Useful for two-player turn-based games.
# This algorithm is slower than alpha-beta.
# It is mostly meant for verifying the correctness of the alpha-beta
# search implementation.

def make(endScore,          # state -> None | score
         heuristicScore,    # state -> score
         currentPlayerGoal, # state -> "max" | "min"
         generateMoves      # state -> (cost,move,nextState) sequence
         ):
    cache = {}

    def evaluate(state,depth):
        if state in cache:
            (cacheDepth,cacheScore) = cache[state]
            if cacheDepth >= depth:
                return cacheScore

        # see first if there's a clear winner or a draw, etc
        score = endScore(state)
        if score == None:
            # have we searched deep enough already?
            if depth <= 0:
                score = heuristicScore(state)
            if score == None:
                (score,_,_) = optimalMoveAux(state,depth)

        # cache the result
        cache[state] = (depth,score)
        return score

    # Decide which move is best for the current player.
    # input: cache is a hash-table
    # returns: (score,move,nextState)
    def optimalMoveAux(state,depth):
        # try every possible move; the value of this state
        # is the value of the best move we could do in this
        # state
        bestMove = None
        bestScore = None
        bestNextState = None
        goal = currentPlayerGoal(state)
        for (cost,move,nextState) in generateMoves(state):
            score = evaluate(nextState,depth-cost)
            if goal == "max":
                if bestScore == None or score > bestScore:
                    bestMove = move
                    bestScore = score
                    bestNextState = nextState
            elif goal == "min":
                if bestScore == None or score < bestScore:
                    bestMove = move
                    bestScore = score
                    bestNextState = nextState
        return (bestScore,bestMove,bestNextState)

    def optimalMove(state,depth):
        (_,move,nextState) = optimalMoveAux(state,depth)
        return (move,nextState)

    return optimalMove
