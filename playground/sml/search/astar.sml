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
    structure CostOrder : ORD_KEY
    sharing type CostMonoid.t = cost
    sharing type CostOrder.ord_key = cost
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
  structure Frontier = ImperativePrioQueue(PrioOrder)

  fun maybeAddState (self as (problem,frontier,depthHash),state,depth,path) =
      let
          val isBestSoFar =
              case HashTable.find depthHash state of
                  NONE => true
                | SOME oldDepth => P.CostOrder.compare (depth,oldDepth) = LESS
      in
          if not isBestSoFar then
              ()
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

  exception NotInDepthHash
  fun begin problem =
      let
          val depthHash = HashTable.mkTable (P.stateHash, P.stateEqual) (4096, NotInDepthHash)
          val frontier = Frontier.make ()
          val self = (problem,frontier,depthHash)
      in
          maybeAddState (self, P.initialState problem, P.CostMonoid.zero, []);
          self
      end

  fun expand (self as (problem,frontier,depthHash),state,depth,path) =
      let
          fun considerMove move =
              let
                  val newDepth = P.CostMonoid.+ (depth, P.moveCost (problem,move))
                  val newState = P.makeMove (problem,move,state)
              in
                  maybeAddState (self,newState,newDepth,move::path)
              end
          val moves = P.possibleMoves (problem, state)
      in
          List.app considerMove moves
      end
          
  fun nextSolution (self as (problem, frontier, depthHash)) =
      if Frontier.null frontier then
          NONE
      else
          let
              val (_,state,depth,path) = Frontier.extract frontier
              val bestDepth = HashTable.lookup depthHash state
          in
              if P.CostOrder.compare (depth, bestDepth) = GREATER then
                  nextSolution (problem, frontier, depthHash)
              else
                  (expand (self,state,depth,path);
                   if P.isGoal (problem,state) then
                       SOME (path, self)
                   else
                       nextSolution self)
          end
end
