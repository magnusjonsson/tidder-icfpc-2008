#lang scheme

(provide push! pop! make empty?)

;Like persistant, but adding sets and boxes
(define (push! x s)
  (set-box! s (cons x (unbox s))))

(define (pop! s)
  (let ((l (unbox s)))
    (set-box! s (cdr l))
    (car l)))

(define (make)
  (box '()))

(define (empty? s)
  (null? (unbox s)))
