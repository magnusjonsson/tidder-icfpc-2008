#lang scheme

(provide update value)

(require "vec2.scm")

(define last-t #f)
(define last-pos #f)

(define speed 0)

(define (value)
  speed)

(define (update t pos)
  (when (and last-t last-pos (> t last-t))
    (set! speed (/ (vec2-distance pos last-pos) (- t last-t))))
  (set! last-t t)
  (set! last-pos pos)
  speed)