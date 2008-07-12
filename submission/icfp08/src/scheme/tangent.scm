#lang scheme

(require "angles.scm")

(provide tangent)

; returns (list x y angle distance) for the tangent point
; direction is -1 for right, 1 for left
(define (tangent robot-x robot-y circle-x circle-y r direction)
  (let* ((dx (- circle-x robot-x))
         (dy (- circle-y robot-y))
         (d (sqrt (+ (* dx dx) (* dy dy))))
         (alpha (asin (/ r d))) ; TODO: if d < r, we're inside the circle and there is no tangent
         (alpha2 (* direction alpha))
         (tangent-angle (+ (atan dy dx) alpha2))
         (tangent-distance (sqrt (- (* d d) (* r r))))
         (tangent-x (* (cos tangent-angle) tangent-distance))
         (tangent-y (* (sin tangent-angle) tangent-distance)))
    (list tangent-x tangent-y (rad->deg tangent-angle) tangent-distance)))
