#! /usr/bin/env mzscheme

#lang scheme

(require "astar.scm")

(let* ((target-x 253)
       (target-y 252)
       (result
        (time
         (a*
          (cons 0 0)
          (lambda (x) (equal? x (cons target-x target-y)))
          (lambda (state)
            (let ((x (car state))
                  (y (cdr state)))
              (list
               (cons (- x 3) y)
               (cons (+ x 5) y)
               (cons x (+ y 3))
               (cons x (- y 2)))))
          (lambda (state)
            (+ (ceiling (abs (/ (- target-x (car state)) 5)))
               (ceiling (abs (/ (- target-y (cdr state)) 3)))))))))
  (printf "~a~n~a~n" (length result) result))
