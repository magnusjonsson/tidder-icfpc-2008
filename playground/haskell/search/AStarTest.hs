{-# LANGUAGE BangPatterns #-}
import qualified AStar as A

start = (0,0) :: (Int,Int)
goal = (253,252)

heuristic !(x,y) =
    case goal of (tx,ty) -> (abs(x-tx)+4) `div` 5 + (abs(y-ty)+2) `div` 3                  

data Move = U | D | L | R deriving Show

possibleMoves state = [U,D,L,R]

moveCost move = 1

makeMove move !(x,y) =
    case move of
      L -> (x-3,y)
      R -> (x+5,y)
      U -> (x,y+3)
      D -> (x,y-2)

problem = A.Problem
          { A.initState = start
          , A.isGoal = (goal==)
          , A.heuristic = heuristic
          , A.possibleMoves = possibleMoves
          , A.moveCost = moveCost
          , A.makeMove = makeMove
          }

main = print (head (A.solutions problem))