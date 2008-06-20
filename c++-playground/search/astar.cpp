#include <vector>
#include <queue>
#include <map>
#include <cstdio>
#include <iostream>
#include <ext/hash_map>

template<class State, class Move>
class Problem {
public:
  virtual State initialState() = 0;
  virtual bool isGoal(State const& state) = 0;
  virtual double heuristic(State const& state) = 0;
  virtual std::vector<Move> possibleMoves(State const& state) = 0;
  virtual double moveCost(Move const& move) = 0;
  virtual State makeMove(State const& state, Move const& move) = 0;
};

int balance = 0;

template<class State, class Move>
class AStar {
private:
  class ConsPath {
  public:
    ConsPath(Move& m, ConsPath* n)
      : move(m)
      , next(n)
      , refcount(1)
    {
      balance++;
      if (n) {
        n->incref();
      }
    }
    ~ConsPath() {
      if (next)
        next->decref();
      balance--;
    }
    Move move;
    ConsPath* next;
    int refcount;
    void incref() {
      ++refcount;
    }
    void decref() {
      if (--refcount == 0)
        delete this;
    }
  };
  class Item {
  public:
    Item(double pr, double d, State const& s, ConsPath* pa)
      : priority(pr)
      , depth(d)
      , state(s)
      , path(pa)
    {
    }
    double priority;
    double depth;
    State state;
    ConsPath* path;
    
    bool operator<(const Item& other) const {
      return priority > other.priority;
    }
  };

  Problem<State,Move>* problem;
  std::priority_queue<Item> frontier;
  __gnu_cxx::hash_map<State,double> depthMap;

  void add(double depth, State const& state, ConsPath* path) {
    if (depthMap.count(state) == 0 || depth < depthMap[state]) {
      depthMap[state] = depth;
      double h = problem->heuristic(state);
      frontier.push(Item(depth+h, depth, state, path));
    }
    else {
      if (path) {
        path->decref();
      }
    }
  }

public:
  AStar(Problem<State,Move>* p) {
    problem = p;
    add(0, problem->initialState(), NULL);
  }
  ~AStar() {
    while(!frontier.empty()) {
      ConsPath* p = frontier.top().path;
      if (p)
        p->decref();
      frontier.pop();
    }
  }
  std::vector<Move>* nextSolution() {
    while(!frontier.empty()) {
      Item item = frontier.top();
      frontier.pop();
      double depth = item.depth;
      State& state = item.state;
      ConsPath* path = item.path;

      if (depth == depthMap[state]) {
        std::vector<Move> moves = problem->possibleMoves(state);
        for(int i=0;i<moves.size();i++) {
          Move& move = moves[i];
          State nextState = problem->makeMove(state,move);
          add(depth+problem->moveCost(move),
              nextState,
              new ConsPath(move, path));
        }
        if (problem->isGoal(state)) {
          std::cout << state.first << " " << state.second << std::endl;
          std::vector<Move>* result = new std::vector<Move>();
          ConsPath* p = path;
          while(p) {
            result->push_back(p->move);
            p = p->next;
          }
          if (path) {
            path->decref();
          }
          return result;
        }
      }
      if (path) {
        path->decref();
      }
    }
    return NULL;
  }
};

#include <algorithm>
#include <cassert>

namespace __gnu_cxx {
  template<>
  struct hash<std::pair<int, int> > {
    size_t operator()(const std::pair<int, int>& p) const {
      return p.first*1374152+p.second;
    }
  };
}

class P : public Problem<std::pair<int,int>, char> {
public:
  typedef std::pair<int,int> State;
  typedef char Move;
  State target;
  P()
    : target(253,252)
  {
  }
  State initialState() { return std::make_pair(0,0); }
  bool isGoal(State const& state) { return state == target; }
  double heuristic(State const& state) {
    return
      (std::abs(state.first-target.first)+4)/5 +
      (std::abs(state.second-target.second)+2)/3;          
  }
  std::vector<Move> possibleMoves(State const& state) {
    std::vector<Move> moves;
    moves.push_back('u');
    moves.push_back('d');
    moves.push_back('l');
    moves.push_back('r');
    return moves;
  }
  double moveCost(Move const& move) { return 1; }
  State makeMove(State const& state, Move const& move) {
    if (move == 'l')  return std::make_pair(state.first-3,state.second);
    if (move == 'r')  return std::make_pair(state.first+5,state.second);
    if (move == 'u')  return std::make_pair(state.first,state.second+3);
    if (move == 'd')  return std::make_pair(state.first,state.second-2);
    assert(false);
    throw 0;
  }
};

int main() {
  {
    P p;
    AStar<P::State,P::Move> a(&p);
    std::vector<char>* sol = a.nextSolution();
    if (sol) {
      printf("%i\n",sol->size());
      for(int i=0;i<sol->size();i++) {
        printf("%c",(*sol)[i]);
      }
      printf("\n");
      delete sol;
    }
  }
  printf("new - delete: %i\n",balance);
  return 0;
}
