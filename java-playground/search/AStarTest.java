import java.util.*;

class AStarTest {
    public class State {
        int x, y;
        State(int _x, int _y) {
            x = _x;
            y = _y;
        }

        public boolean equals(Object other) {
            if (other == null)
                return false;
            if (!(other instanceof State))
                return false;
            State o = (State)other;
            return x == o.x && y == o.y;
        }
        public int hashCode() {
            return x * 12358123 + y * 385912384;
        }
    };
    
    public AStarTest() {
        SearchProblem p = new SearchProblem() {
                State target = new State(253, 252);
                public Object initialState() {
                    return new State(0,0);
                }
                public boolean isGoal(Object o) {
                    return target.equals(o);
                } 
                public double heuristic(Object o) {
                    State s = (State)o;
                    return (Math.abs(s.x-target.x)+4)/5
                        +  (Math.abs(s.y-target.y)+2)/3;
                }
                public Iterable<Object> possibleMoves(Object state) {
                    List<Object> moves = new ArrayList<Object>();
                    moves.add("up");
                    moves.add("down");
                    moves.add("left");
                    moves.add("right");
                    return moves;
                }
                public double moveCost(Object move) {
                    return 1;
                }
                public Object makeMove(Object state, Object move) {
                    State s = (State)state;
                    String m = (String)move;
                    if (m.equals("left")) {
                        return new State(s.x-3, s.y);
                    }
                    if (m.equals("right")) {
                        return new State(s.x+5, s.y);
                    }
                    if (m.equals("down")) {
                        return new State(s.x, s.y-2);
                    }
                    if (m.equals("up")) {
                        return new State(s.x, s.y+3);
                    }
                    assert(false);
                    return null;
                }
            };
        AStar a = new AStar(p);
        List<Object> sol = a.nextSolution();
        System.out.println(sol.size());
        for(Object o : sol) {
            String s = (String)o;
            System.out.println(s);
        }
    }


    public static void main(String[] args) {
        new AStarTest();
    }
};