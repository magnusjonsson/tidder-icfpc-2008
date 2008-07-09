module AStar (solutions) where

import qualified Data.Map as Map
import qualified Data.List as List

solutions initState isGoal heuristic possibleMoves = 
    loop (add initState 0 [] emptyState)
    where
      emptyState = (Map.empty,Map.empty)
      loop (frontier,bestDepth) =
          if Map.null frontier then []
          else case deleteFindMinOne frontier of
                 ((_,(state,depth,path)),frontier) ->
                     let
                         addMove searchState (cost,move,nextState) =
                             add nextState (depth+cost) (move:path) searchState

                         nextSearchState =
                             List.foldl' addMove (frontier,bestDepth)
                                         (possibleMoves state)

                         rest = loop nextSearchState
                     in 
                       if isGoal state then (List.reverse path:rest) else rest
      add state depth path (searchState @ (frontier,bestDepth)) =
          let doAdd =
                  case depth+heuristic state of
                    priority ->
                        case insertWith' (++) priority [(state,depth,path)] frontier of
                          frontier ->
                              case minsert state depth bestDepth of
                                bestDepth ->
                                    (frontier,bestDepth)
          in
            case mlookup state bestDepth of
              Nothing -> doAdd
              Just previousDepth ->
                  if depth < previousDepth then doAdd else searchState

{- Below are wrappers for Map's functions for profiling purposes. -}

deleteFindMin a = Map.deleteFindMin a
mlookup a b = Map.lookup a b
minsert a b c = Map.insert a b c
insertWith' a b c d = Map.insertWith' a b c d
updateMin a b = Map.updateMin a b


{- Get one item out of the heap -}
{-# INLINE deleteFindMinOne #-}
deleteFindMinOne::Ord k => Map.Map k [v] -> ((k,v), Map.Map k [v])
deleteFindMinOne map =
    case deleteFindMin map of
      ((key, [item]),map) -> ((key,item),map)
      ((key, item:items),_) ->
          case updateMin (const (Just items)) map of
            map -> ((key,item), map)
