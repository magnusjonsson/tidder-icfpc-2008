#lang scheme

(require "angles.scm")

(provide tangent tangent2)

; returns (values x y angle distance) for the tangent point
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
    (values tangent-x tangent-y (rad->deg tangent-angle) tangent-distance)))

(define (tangent2 robot-x robot-y circle-x circle-y r direction)
  (let* ((dx (- circle-x robot-x))
         (dy (- circle-y robot-y))
         (d2 (+ (* dx dx) (* dy dy)))
         (d  (sqrt d2)) ; avoid producing imaginary numbers
         (r (min d r)) ; reduce radius if we're inside the radius already
         (r2 (* r r))
         (t2 (- d2 r2))
         (t  (sqrt (abs t2))) ; avoid producing imaginary numbers
         (cos (/ t d))
         (sin (/ r d))
         (tx (+ robot-x (* cos (+ (* dx cos) (* -1 dy direction sin)))))
         (ty (+ robot-y (* cos (+ (* dy cos) (* dx direction sin) )))))
    (values tx ty #f t)))