#! /usr/bin/env mzscheme

#lang scheme

(require "astar.ss")
(require "iterative-deepening-astar.ss")

(require (only-in rnrs/base-6 assert))

(define (test-search-fn search-fn)
  (call/cc
   (lambda (k)
     (let ((target-x 253)
           (target-y 252))
     (search-fn (cons 0 0)
                (lambda (state)
                  (and (= (car state) target-x)
                       (= (cdr state) target-y)))
                (lambda (state)
                  (+ (ceiling (abs (/ (- (car state) target-x) 3)))
                     (ceiling (abs (/ (- (cdr state) target-y) 5))))
;                  0
                  )
                (lambda (state)
                  (list (cons 1 'up)
                        (cons 1 'down)
                        (cons 1 'left)
                        (cons 1 'right)))
                (lambda (state move)
                  (let ((x (car state))
                        (y (cdr state)))
                    (case (cdr move)
                      ('left (cons (- x 3) y))
                      ('right (cons (+ x 5) y))
                      ('up (cons x (+ y 3)))
                      ('down (cons x (- y 2)))
                      (else (error "invalid move")))))
                (lambda (solution)
                  (k solution)))))))

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