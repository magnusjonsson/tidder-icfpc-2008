signature PROBLEM = sig
    type state
    type instance
    type move
    type cost
    structure CostMonoid : sig
      type t
      val + : t * t -> t
      val zero : t
    end
    sharing type CostMonoid.t = cost
    val initialState : instance -> state
    val isGoal : instance * state -> bool
    val heuristic : instance * state -> cost
    val possibleMoves : instance * state -> move list
    val moveCost : instance * move -> cost
    val makeMove : instance * move * state -> state
    val stateHash : state -> word
    val stateEqual : state * state -> bool
end

functor AStar (P : PROBLEM) = struct
  structure PrioOrder = struct
    type ord_key = P.cost * P.state * P.cost * P.move list
    fun compare ((a,_,_,_),(c,_,_,_)) =
        P.CostOrder.compare (a,c)
  end
  structure Frontier = PrioQueue(PrioOrder)
  exception NotInDepthHash

  fun maybeAddState (problem,frontier,depthHash,state,depth,path) =
      let
          val isBestSoFar =
              case HashTable.find depthHash state of
                  NONE => true
                | SOME oldDepth => P.CostOrder.compare (depth,oldDepth) = LESS
      in
          if not isBestSoFar then
              frontier
          else
              let
                  val estimatedGoalDepth =
                      P.CostMonoid.+ (depth, P.heuristic (problem, state))
                  val queueItem =
                      (estimatedGoalDepth, state, depth, path)
              in
                  HashTable.insert depthHash (state,depth);
                  Frontier.insert (frontier,queueItem)
              end
      end

  fun begin problem =
      let
          val depthHash = HashTable.mkTable (P.stateHash, P.stateEqual) (4096, NotInDepthHash)
          val frontier =
              maybeAddState (problem, Frontier.empty, depthHash,
                             P.initialState problem, P.CostMonoid.zero, [])
      in
          (problem,frontier,depthHash)
      end

  fun considerMove (problem,depthHash,state,depth,path) (move,frontier) =
      let
          val newDepth = P.CostMonoid.+ (depth, P.moveCost (problem,move))
          val newState = P.makeMove (problem,move,state)
      in
          maybeAddState (problem,frontier,depthHash,newState,newDepth,move::path)
      end

  fun expand (problem,frontier,depthHash,state,depth,path) =
      let
          val moves = P.possibleMoves (problem, state)
      in
          foldl (considerMove (problem,depthHash,state,depth,path)) frontier moves
      end
          
  fun nextSolution (problem, frontier, depthHash) =
      if Frontier.null frontier then
          NONE
      else
          case Frontier.extract frontier of
              ((_,state,depth,path), frontier) =>
              case HashTable.find depthHash state of
                  NONE => raise Fail "Impossible case"
                | SOME bestDepth =>
                  if P.CostOrder.compare (depth, bestDepth) = GREATER then
                      nextSolution (problem, frontier, depthHash)
                  else
                      let
                          val frontier =
                              expand (problem,frontier,depthHash,state,depth,path)
                      in
                          if P.isGoal (problem,state) then
                              SOME (path, (problem, frontier, depthHash))
                          else
                              nextSolution (problem, frontier, depthHash)
                      end
end
