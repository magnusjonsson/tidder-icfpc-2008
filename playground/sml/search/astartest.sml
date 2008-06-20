structure AStarTest = struct

  structure Problem = struct
    type state = int * int
    type instance = state    (* the goal *)
    datatype move = U | D | L | R
    type cost = int
    structure CostOrder = struct type ord_key = int val compare = Int.compare end
    structure CostMonoid = struct type t = int val op+ = Int.+ val zero = 0 end
                           
    fun initialState _ = (0,0)
    fun isGoal (goal:state,state:state) = state = goal
    fun heuristic (goal:state,(x,y):state) =
        let val (tx,ty) = goal in
            ((abs(x-tx)+0) div 5) + ((abs(y-ty)+0) div 3)
        end
    fun possibleMoves _ = [U,D,L,R]
    fun moveCost _ = 1
    fun makeMove (_,move,(x,y)) =
        case move of L => (x-3,y)
                   | R => (x+5,y)
                   | U => (x,y+3)
                   | D => (x,y-2)
                          
    fun stateHash ((x,y):state) =
        Word.fromInt (x * 171179 + y)
    fun stateEqual (a:state,b:state) = a = b
  end
                      
  structure Solver = AStar(Problem)
                     
  fun solve goal =
      case Solver.nextSolution (Solver.begin goal) of
          NONE => NONE
        | SOME (solution,_) => SOME solution
                               
  fun test goal =
      case Solver.nextSolution (Solver.begin goal) of
          NONE => raise Fail "No solution.\n"
        | SOME (solution, _) =>
          (TextIO.print "Found solution of length ";
           TextIO.print (Int.toString (length solution));
           TextIO.print "\n")
end
