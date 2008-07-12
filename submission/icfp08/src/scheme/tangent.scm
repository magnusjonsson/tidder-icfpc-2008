#lang scheme

(require "angles.scm")

(provide tangent)

; returns (values x y angle distance) for the tangent point
; direction is -1 for right, 1 for left
(define (tangent robot-x robot-y circle-x circle-y r direction)
  (let* ((dx (- circle-x robot-x))
         (dy (- circle-y robot-y))
         (d (sqrt (+ (* dx dx) (* dy dy))))
         (d2 (max r d)) ; use this for the fake tangent
         (alpha (asin (/ r d2)))
         (alpha2 (* direction alpha))
         (tangent-angle (+ (atan dy dx) alpha2))
         (tangent-distance (if (<= d r) 1 (sqrt (- (* d d) (* r r))))) ; fake distance 1 if too near
         (tangent-x (+ (* (cos tangent-angle) tangent-distance) robot-x))
         (tangent-y (+ (* (sin tangent-angle) tangent-distance) robot-y)))
    (values tangent-x tangent-y (rad->deg tangent-angle) tangent-distance)))
