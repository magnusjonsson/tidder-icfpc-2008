import alphabeta
import minimax

# note:
# in tic-tac-toe, the end games in a draw if
# both players play optimally

def getRow(grid,x,y,dx,dy):
    result = []
    for i in range(0,3):
        result.append(grid[y][x])
        x += dx
        y += dy
    return ''.join(result)

def getAllRows(grid):
    # horizontal
    yield getRow(grid,0,0,1,0)
    yield getRow(grid,0,1,1,0)
    yield getRow(grid,0,2,1,0)
    # vertical
    yield getRow(grid,0,0,0,1)
    yield getRow(grid,1,0,0,1)
    yield getRow(grid,2,0,0,1)
    # diagonal
    yield getRow(grid,0,0,1,1)
    yield getRow(grid,2,0,-1,1)

def getEmptyCells(grid):
    for y in range(0,3):
        for x in range(0,3):
            if grid[y][x] == '.':
                yield (x,y)

startState = ('x', ('...',
                    '...',
                    '...'))

def updateTuple(t, index, newValue):
    result = list(t)
    result[index] = newValue
    return tuple(result)
def updateStr(t, index, newValue):
    result = list(t)
    result[index] = newValue
    return ''.join(result)

def play(state,move):
    (whosturn,grid) = state
    (x,y) = move
    assert grid[y][x] == '.'

    grid = updateTuple(grid,y,updateStr(grid[y],x,whosturn))
    if whosturn == 'x':
        whosturn = 'o'
    elif whosturn == 'o':
        whosturn = 'x'
    else:
        raise 'bleep'
    return (whosturn,grid)

def generateMoves(state):
    (whosturn,grid) = state
    for move in getEmptyCells(grid):
        yield (1,move,play(state,move))

# returns None if the game has not yet ended.
# otherwise 10000 if current player won
#          -10000 if current player lost
#               0 if it is a draw
def endScore(state):
    (whosturn,grid) = state
    score = None
    for row in getAllRows(grid):
        if row == 'xxx':
            score = 10000
            break
        if row == 'ooo':
            score = -10000
            break
        
    if score == None:
        try:
            getEmptyCells(grid).next()
            return None
        except StopIteration:
            score = 0 # draw

    return score

# the better the state seems for the current player,
# the higher a number should this function return.
def heuristic(state):
    # we can assume here that the game is not over
    # because quickHeuristic has already been called
    (whosturn,grid) = state
    score = 0

    for row in getAllRows(grid):
        numx = 0
        numo = 0
        for mark in row:
            if mark == 'x': numx += 1
            elif mark == 'o': numo += 1
        if numo == 0: score += 1 # score += numx is better, but it istoo good for testing the game tree search
        if numx == 0: score -= 1 # score -= numo ...
    if whosturn == 'x':
        score += 2
    elif whosturn == 'o':
        score -= 2
    else:
        raise 'blaa2'
    return score


def currentPlayerGoal(state):
    (whosturn, grid) = state
    if whosturn == 'x':
        return 'max'
    elif whosturn == 'o':
        return 'min'
    else:
        raise 'strange state'

def playGame():
    state = startState
    optimalMove1 = alphabeta.make(endScore,heuristic,currentPlayerGoal,generateMoves)
    optimalMove2 = minimax.make(endScore,heuristic,currentPlayerGoal,generateMoves)
    print state
    depth = 4
    while endScore(state) == None:
        m1 = optimalMove1(state,depth)
        m2 = optimalMove2(state,depth)
        assert m1 == m2
        (move,state) = m1
        print state
    print 'final result: ', endScore(state)

        
playGame()
