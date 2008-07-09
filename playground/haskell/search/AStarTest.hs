module Main () where
import qualified AStar as A

start = (0,0) :: (Int,Int)
goal = (253,252)

data Move = U | D | L | R deriving Show

possibleMoves (x,y) =
    [(1,U,(x,y+3))
    ,(1,D,(x,y-2))
    ,(1,L,(x-3,y))
    ,(1,R,(x+5,y))
    ]

heuristic (x,y) =
    case goal of
      (tx,ty) -> (abs(x-tx)+4) `div` 5 + (abs(y-ty)+2) `div` 3

main = print (head (A.solutions start (goal==) heuristic possibleMoves))