import java.util.*;

interface SearchProblem {
    Object initialState();
    boolean isGoal(Object state);
    double heuristic(Object state);
    Iterable<Object> possibleMoves(Object state);
    double moveCost(Object move);
    Object makeMove(Object state, Object move);
}

class AStar implements Iterable<List<Object>> {
    class ConsPath {
        public Object car;
        public ConsPath cdr;
        public ConsPath(Object a, ConsPath d) {
            car = a;
            cdr = d;
        }
    };

    class Item {
        double priority;
        double depth;
        Object state;
        ConsPath path;
        public Item(double priority, double depth, Object state, ConsPath path) {
            this.priority = priority;
            this.depth = depth;
            this.state = state;
            this.path = path;
        }
    };

    private SearchProblem problem;
    private PriorityQueue<Item> frontier;
    private HashMap<Object,Double> depthMap =
        new HashMap<Object,Double>();

    public AStar(SearchProblem problem) {
        this.problem = problem;
        this.frontier = new PriorityQueue<Item>
            (1024, new Comparator<Item>() {
                public int compare(Item a, Item b) {
                    int ord = Double.compare(a.priority,b.priority);
                    /*
                    if (ord == 0)
                       ord = Double.compare(b.depth,a.depth);
                    */
                    return ord;
                }
            });
        add(0, problem.initialState(), null);
    }

    // returns null if there is no more solution
    public List<Object> nextSolution() {
        while(!frontier.isEmpty()) {
            Item item = frontier.poll();
            Object state = item.state;
            double depth = item.depth;
            ConsPath path = item.path;
            for (Object move : problem.possibleMoves(state)) {
                double nextDepth = depth + problem.moveCost(move);
                Object nextState = problem.makeMove(state,move);
                add(nextDepth, nextState, new ConsPath(move, path));
            }
            if (problem.isGoal(state)) {
                ArrayList<Object> solution = new ArrayList<Object>();
                ConsPath p = path;
                while(p != null) {
                    solution.add(p.car);
                    p = p.cdr;
                }
                return solution;
            }
        }
        return null;
    }

    private void add(double depth, Object state, ConsPath path) {
        Double d = depthMap.get(state);
        if (d == null || depth < d.doubleValue()) {
            double h = problem.heuristic(state);
            depthMap.put(state,new Double(depth));
            frontier.add(new Item(depth+h, depth, state, path));
        }
    }

    public Iterator<List<Object>> iterator() {
        return new Iterator<List<Object>>() {
            private boolean done = false;
            private List<Object> solution;

            public boolean hasNext() {
                if (done)
                    return false;
                if (solution == null)
                    solution = nextSolution();
                return solution != null;
            }
            public List<Object> next() {
                List<Object> ret = solution;
                solution = null;
                return ret;
            }
            public void remove() {
            }
        };
    }
};