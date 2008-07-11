#lang scheme

(provide push pop empty empty?)

;Trivial
(define push cons)
(define get car)
(define pop cdr)
(define empty '())
(define empty? null?)
