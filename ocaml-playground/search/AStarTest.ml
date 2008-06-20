module Problem = struct
  type instance = unit
  type state = int * int
  type move = Left | Right | Up | Down
  module Cost = struct
    type t = int
    let compare (a,b) = a-b
    let add (a,b) = a+b
    let zero = 0
  end

  let goal = (243,242)
  let initial_state () = (0,0)
  let is_goal (_,state) = state = goal
  let move_cost (_,move) = 1
  let possible_moves (_,_) = [Up;Down;Left; Right]
  let heuristic (_,(x,y)) =
    let (tx,ty) = goal in (abs(x-tx)+4)/5 + (abs(y-ty)+2)/3
  let make_move (_,(x,y),move) =
    match move with
      | Left  -> (x-3,y)
      | Right -> (x+5,y)
      | Up    -> (x,y+3)
      | Down  -> (x,y-2)
end

module Solver = AStar.Make(Problem)

;;
let solver = Solver.make ()
;;
match Solver.next_solution solver with
    None -> print_string "No solution!\n"
  | Some (state,path,solver) ->
      print_string ("Found solution of length "^string_of_int (List.length path)^"\n")
