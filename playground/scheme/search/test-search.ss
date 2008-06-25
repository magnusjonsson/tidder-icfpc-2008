#! /usr/bin/env mzscheme

#lang scheme

(require "astar.ss")
(require "iterative-deepening-astar.ss")

(require (only-in rnrs/base-6 assert))

(define (test-search-fn search-fn)
  (let/ec return
    (let ((target-x 253)
          (target-y 252))
      (search-fn (cons 0 0)
                 (lambda (state)
                   (and (= (car state) target-x)
                        (= (cdr state) target-y)))
                 (lambda (state)
                   (+ (quotient (+ (abs (- (car state) target-x)) 4) 5)
                      (quotient (+ (abs (- (cdr state) target-y)) 2) 3)))
                 (lambda (state yield!)
                   (let ((x (car state))
                         (y (cdr state)))
                     (yield! 1 'up    (cons x (+ y 3)))
                     (yield! 1 'down  (cons x (- y 2)))
                     (yield! 1 'left  (cons (- x 3) y))
                     (yield! 1 'right (cons (+ x 5) y))))
                 (lambda (solution)
                   (return solution))))))

(define (test)
  (let ((sol1 (time (test-search-fn a*)))
        (sol2 (time (test-search-fn ida*))))
    (printf "sol1 length: ~s~n" (length sol1))
    (printf "sol2 length: ~s~n" (length sol2))
    (printf "sol1: ~s~n" sol1)
    (printf "sol2: ~s~n" sol2)
    (assert (= (length sol1)
               (length sol2)))))

(test)