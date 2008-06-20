module type PROBLEM = sig
  type instance
  type state
  type move
  module Cost : sig
    type t
    val compare : t * t -> int
    val add     : t * t -> t
    val zero    : t
  end
  val initial_state : instance -> state
  val is_goal : instance * state -> bool
  val heuristic : instance * state -> Cost.t
  val possible_moves : instance * state -> move list
  val make_move : instance * state * move -> state
  val move_cost : instance * move -> Cost.t
end

module type S = functor (P : PROBLEM) -> sig
  type t
  val make : P.instance -> t
  val next_solution : t -> (P.state * P.move list * t) option
end

module Make : S = functor (P : PROBLEM) -> struct
  module Cost = P.Cost
  type state = P.state
  type move = P.move
  type item = Cost.t *  Cost.t * state * move list
  module ItemOrder = struct
    type t = item
    let compare (prio1,_,_,_) (prio2,_,_,_) =
      Cost.compare (prio1,prio2)
  end
  module ItemQueue = PrioQueue.Make(ItemOrder)

  type t = P.instance * ItemQueue.t * (P.state,Cost.t) Hashtbl.t
  let add (state, depth, (path : move list), instance, frontier, depthHash) =
    let should_enqueue =
      try Cost.compare (depth, Hashtbl.find depthHash state) < 0
      with Not_found -> true
    in
      if should_enqueue then
        let h = P.heuristic (instance, state) in
          Hashtbl.replace depthHash state depth;
          ItemQueue.insert (Cost.add(depth,h), depth, state,  path) frontier
      else
        frontier

  let make instance =
    let depthHash = Hashtbl.create 32768 in
      (instance,
       add (P.initial_state instance,
            P.Cost.zero,
            [],
            instance,
            ItemQueue.empty,
            depthHash),
       depthHash)
  let rec next_solution (instance,frontier,depthHash) =
    if ItemQueue.is_empty frontier then None
    else match ItemQueue.extract frontier with
        (_,depth,state,path), frontier ->
          if depth > Hashtbl.find depthHash state then None
          else
            let enqueue frontier move =
              let next_state = P.make_move (instance,state,move) in
              let move_cost = P.move_cost (instance,move) in
                add (next_state, Cost.add(depth,move_cost), move::path, instance, frontier, depthHash)
            in
            let frontier = 
              List.fold_left enqueue frontier (P.possible_moves (instance,state))
            in
              if P.is_goal (instance,state) then
                Some (state,path,(instance,frontier,depthHash))
              else
                next_solution (instance,frontier,depthHash)
end
