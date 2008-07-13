#lang scheme

(provide update value)

(require "angles.scm")

(define last-t #f)
(define last-angle #f)

(define speed 0)

(define (value)
  speed)

(define (update t angle)
  (when (and last-t last-angle (> t last-t))
    (set! speed (/ (deg- angle last-angle) (- t last-t))))
  (set! last-t t)
  (set! last-angle angle)
  speed)