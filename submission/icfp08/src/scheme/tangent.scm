#lang scheme

(require "angles.scm")

(provide tangent tangent2)

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

(define (tangent2 robot-x robot-y circle-x circle-y r direction)
  (let* ((dx (- circle-x robot-x))
         (dy (- circle-y robot-y))
         (d2 (+ (* dx dx) (* dy dy)))
         (d  (sqrt d2)) ; avoid producing imaginary numbers
         (r2 (* r r)))
    (if (<= d2 r2)
        (let ((tx (+ robot-x (* -1 dy direction)))
              (ty (+ robot-y (* dx direction))))
          (values tx ty #f d))
        (let* ((t2 (- d2 r2))
               (t  (sqrt t2)) ; avoid producing imaginary numbers
               (cos (/ t d))
               (sin (/ r d))
               (tx (+ robot-x (* cos (+ (* dx cos) (* -1 dy direction sin)))))
               (ty (+ robot-y (* cos (+ (* dy cos) (* dx direction sin) )))))
          (values tx ty #f t)))))