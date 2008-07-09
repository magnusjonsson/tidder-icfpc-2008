{-# LANGUAGE BangPatterns #-}
module AStar (solutions, Problem(..)) where

import qualified Data.Map as Map

deleteFindMinOne::Ord k => Map.Map k [v] -> ((k,v), Map.Map k [v])
deleteFindMinOne map =
    case deleteFindMin map of
      ((key, [item]),map) -> ((key,item),map)
      ((key, item:items),_) ->
          case updateMin (const (Just items)) map of
            map -> ((key,item), map)

data Item state move cost = Item !state !cost ![move]

newtype (Ord state, Num cost) => AStarState state move cost =
    S (Map.Map cost [Item state move cost], Map.Map state cost)

type AStarStateTransformer state move cost =
    AStarState state move cost -> AStarState state move cost

data (Ord state, Num cost, Ord cost) => Problem state move cost =
    Problem { initState     :: state
            , isGoal        :: state -> Bool
            , heuristic     :: state -> cost
            , possibleMoves :: state -> [move]
            , moveCost      :: move -> cost
            , makeMove      :: move -> state -> state
            }

solutions::(Ord state, Num cost, Ord cost) => Problem state move cost -> [[move]]
solutions !problem = 
    loop problem (add problem (initState problem) 0 [] emptyState)
         where emptyState = S (Map.empty,Map.empty)

add !problem !state !depth !path !(searchState @ (S (frontier,bestDepth))) =
    case mlookup state bestDepth of
      Nothing -> doAdd
      Just previousDepth ->
          if depth < previousDepth then
              doAdd
          else
              searchState
    where
      doAdd =
          S (insertWith' (++) (depth+heuristic problem state) [Item state depth path] frontier
            ,insert2 state depth bestDepth
            )

loop !problem !(S (frontier,bestDepth)) =
    if Map.null frontier then []
    else case deleteFindMinOne frontier of
           ((_,Item state depth path),frontier) ->
               case mlookup state bestDepth of
                 Nothing -> error "Impossible"
                 Just depth' ->
                     if depth /= depth' then
                         loop problem (S (frontier, bestDepth))
                     else
                         let addMove !searchState !move =
                                 let nextDepth = depth + moveCost problem move
                                     nextState = makeMove problem move state
                                 in
                                   add problem nextState nextDepth (move:path) searchState
                             addMoves !searchState ![] = searchState
                             addMoves !searchState !(move:moves) =
                                 addMoves (addMove searchState move) moves
                             nextSearchState = addMoves (S (frontier,bestDepth)) (possibleMoves problem state)                                     
                             rest = loop problem nextSearchState
                         in 
                           if isGoal problem state then (reverse path:rest) else rest

{- Below are wrappers for Map's functions. This is for profiling purposes. -}

deleteFindMin a = Map.deleteFindMin a

mlookup !a !b = Map.lookup a b
insert1 !a !b !c = Map.insert a b c
insert2 !a !b !c = Map.insert a b c
insertWith' !a !b !c !d = Map.insertWith' a b c d
updateMin !a !b = Map.updateMin a b