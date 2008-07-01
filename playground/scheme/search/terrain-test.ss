#! /usr/bin/env mzscheme

#lang scheme

(require "astar.ss")
(require "iterative-deepening-astar.ss")

(require (only-in rnrs/base-6 assert))

(define width 400)
(define height 400)

(define terrain (build-vector (* width height) (lambda (i) (+ 0.2 (random)))))

(define start (cons 0 0))

(define goal (cons (sub1 width) (sub1 height)))

(define (goal? state) (equal? state goal))

(define (heuristic-lower-bound state)
    (* 0.2 (+ (abs (- (car state) (car goal)))
              (abs (- (cdr state) (cdr goal))))))

(define (in-bounds? x y)
  (and (>= x 0) (< x width)
       (>= y 0) (< y height)))

(define (terrain-ref x y)
  (vector-ref terrain (+ x (* y 30))))

(define (generate-moves! state k)
  (let ((x (car state))
        (y (cdr state))
        (generate-move! (lambda (x y dir)
                          (when (in-bounds? x y)
                            (k (terrain-ref x y) dir (cons x y))))))
    (generate-move! (sub1 x) y 'left)
    (generate-move! (add1 x) y 'right)
    (generate-move! x (add1 y) 'up)
    (generate-move! x (sub1 y) 'down)))

(define (test-search-fn search-fn)
  (let/ec return
      (search-fn start
                 goal?
                 heuristic-lower-bound
                 generate-moves!
                 (lambda (solution cost)
                   (return solution)))))

(define (test)
  (let ((sol (time (test-search-fn a*))))
    (printf "sol length: ~s~n" (length sol))
    (printf "sol: ~s~n" sol)))

(test)