#lang scheme

(require "minimax.ss")
(require "alpha-beta.ss")
(require srfi/43)     ; vector-copy

; a state is (cons whos-turn 9x9-grid) where 9x9-grid is represented by a
; 9-vector in row major order.

(define start-state (cons 'x (make-vector 9 '_)))

(define (make-state whos-turn grid)
  (cons whos-turn grid))

(define (whos-turn state) (car state))
(define (grid state) (cdr state))

(define (grid-ref g x y)
  (vector-ref g (+ (* y 3) x)))

(define (grid-set! g x y v)
  (vector-set! g (+ (* y 3) x) v))

(define (next-state state move)
  (let ((x (car move))
        (y (cdr move))
        (new-whos-turn (case (whos-turn state) ('x 'o) ('o 'x)))
        (new-grid (vector-copy (grid state))))
    (unless (eq? '_ (grid-ref new-grid x y))
      (error "already occupied"))
    (grid-set! new-grid x y (whos-turn state))
    (cons new-whos-turn new-grid)))

(define (eventual-state state moves)
  (if (null? moves)
      state
      (eventual-state (next-state state (car moves))
                      (cdr moves))))

(define (generate-moves! state k!)
  (let ((g (grid state)))
    (do ((y 0 (add1 y))) ((>= y 3))
      (do ((x 0 (add1 x))) ((>= x 3))
        (when (eq? '_ (grid-ref g x y))
          (let ((move (cons x y)))
            (k! 1 move (next-state state move))))))))

(define (line-end-score g x y dx dy)
  (let ((a (grid-ref g x y))
        (b (grid-ref g (+ x dx) (+ y dy)))
        (c (grid-ref g (+ x dx dx) (+ y dy dy))))
    (cond
      ((and (eq? a 'x) (eq? b 'x) (eq? c 'x)) 10000)
      ((and (eq? a 'o) (eq? b 'o) (eq? c 'o)) -10000)
      (#t #f))))

(define (draw? state)
  (let loop ((i 0))
    (if (< i 9)
        (if (eq? '_ (vector-ref (grid state) i))
            #f
            (loop (add1 i)))
        #t)))

(define (end-score state)
  (let ((g (grid state)))
    (or
     ; horiz
     (line-end-score g  0 0  1 0)
     (line-end-score g  0 1  1 0)
     (line-end-score g  0 2  1 0)
     ; vert
     (line-end-score g  0 0  0 1)
     (line-end-score g  1 0  0 1)
     (line-end-score g  2 0  0 1)
     ; diag \
     (line-end-score g  0 0  1 1)
     ; diag /
     (line-end-score g  2 0 -1 1)
     
     (and (draw? state) 0))))

(define (binary boolean)
  (if boolean 1 0))

(define (line-heuristic g x y dx dy)
  (let ((num-x 0) (num-o 0))
    (let loop ((i 0) (x x) (y y))
      (when (< i 3)
        (case (grid-ref g x y)
          ('x (set! num-x (add1 num-x)))
          ('o (set! num-o (add1 num-o))))
        (loop (add1 i) (+ x dx) (+ y dy))))
    (- (if (and (zero? num-o) (not (zero? num-x))) 1 0)
       (if (and (zero? num-x) (not (zero? num-o))) 1 0))))

(define (heuristic-score state)
  (let ((g (grid state)))
    (+
     ; horiz
     (line-heuristic g  0 0  1 0)
     (line-heuristic g  0 1  1 0)
     (line-heuristic g  0 2  1 0)
     ; vert
     (line-heuristic g  0 0  0 1)
     (line-heuristic g  1 0  0 1)
     (line-heuristic g  2 0  0 1)
     ; diag \
     (line-heuristic g  0 0  1 1)
     ; diag /
     (line-heuristic g  2 0 -1 1)
     ; initiative advantage
     (case (whos-turn state)
       ('x 2)
       ('o -2)))))

(define (current-player-goal state)
  (case (whos-turn state)
    ('x 'max)
    ('o 'min)))


(define (test)
  (let ((minimax (make-minimax end-score
                               heuristic-score
                               current-player-goal
                               generate-moves!))
        (alpha-beta (make-alpha-beta end-score
                                     heuristic-score
                                     current-player-goal
                                     generate-moves!))
        (depth 9))
    (let loop ((state start-state))
      (printf "~a~n" state)
      (if (end-score state)
          (printf "~a~n" (end-score state))
          (let-values (((move-1 next-state-1) (minimax state depth))
                       ((move-2 next-state-2) (alpha-beta state depth)))
            (when (or (not (equal? move-1 move-2))
                      (not (equal? next-state-1 next-state-2)))
              (printf "Minimax and alpha-beta don't behave the same. Bug?~n"))
            (loop next-state-1))))))

(time (test))