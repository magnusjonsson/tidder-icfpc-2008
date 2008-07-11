#lang scheme

;Amortized O(1) push and pop
;Read "Purely Functional Data Structures" for more info

(provide push pop empty empty?)

(define (normalize front back)
  (if (null? front)
      (cons (reverse back) '())
      (cons front back)))

(define (push x q)
  (normalize (car q) (cons x (cdr q))))

(define (get q)
  (car (car q)))

(define (pop q)
  (normalize (cdr (car q)) (cdr q)))

(define empty
  (cons '() '()))

(define (empty? s)
  (null? (car s)))
