#lang scheme

(require "angles.scm")
(require "vec2.scm")
(require (only-in rnrs/base-6 assert))

(provide tangent tangent2 circle-circle-tangent)

(define (tangent robot circle radius direction)
  (let-values (((x y a d)
                (tangent-raw (vec2-x robot) (vec2-y robot)
                             (vec2-x circle) (vec2-y circle)
                             radius
                             direction)))
    (make-vec2 x y)))

; returns (values x y angle distance) for the tangent point
; direction is -1 for right, 1 for left
(define (tangent-raw robot-x robot-y circle-x circle-y r direction)
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

(define (tangent2 robot circle radius direction)
  (let-values (((x y a d)
                (tangent2-raw (vec2-x robot) (vec2-y robot)
                              (vec2-x circle) (vec2-y circle)
                              radius
                              direction)))
    (make-vec2 x y)))

(define (tangent2-raw robot-x robot-y circle-x circle-y r direction)
  (let* ((dx (- circle-x robot-x))
         (dy (- circle-y robot-y))
         (d2 (+ (* dx dx) (* dy dy)))
         (d  (sqrt d2))
         (r2 (* r r)))
    (if (<= d2 r2)
        (let ((tx (+ robot-x (* -1 dy direction)))
              (ty (+ robot-y (* dx direction))))
          (values tx ty #f d))
        (let* ((t2 (- d2 r2))
               (t  (sqrt t2))
               (cos (/ t d))
               (sin (/ r d))
               (tx (+ robot-x (* cos (+ (* dx cos) (* -1 dy direction sin)))))
               (ty (+ robot-y (* cos (+ (* dy cos) (* dx direction sin) )))))
          (values tx ty #f t)))))

; http://mathworld.wolfram.com/Circle-CircleTangents.html
; http://img244.imageshack.us/img244/1309/33535634od7.png
(define (circle-circle-tangent center1 radius1 dir1
                               center2 radius2 dir2)
  (if (< radius2 radius1)
      (let ((result (circle-circle-tangent center2 radius2 (- dir2)
                                           center1 radius1 (- dir1))))
        (cons (cdr result) (car result)))
      ; assume radius1 smaller:
      (let* ((p1 center1)
             (p2 (tangent p1 center2 (- radius2 (* dir1 dir2 radius1)) dir2))
             (diff (vec2- p2 p1))
             (radius-dir (vec2-normalize (vec2-rotate-ccw-90 diff)))
             (adjustment (vec2-scale (* dir1 radius1) radius-dir))
             (p1 (vec2+ p1 adjustment))
             (p2 (vec2+ p2 adjustment)))
        (cons p1 p2))))



(define (test)
  (define (ex x msg2)
    (printf "~a~n" x)
    (printf "should be ~a~n~n" msg2))
  (ex (circle-circle-tangent (make-vec2 0 1) 1 -1
                             (make-vec2 10 2) 2 -1)
      "(0,0) (10,0)")

  (ex (circle-circle-tangent (make-vec2 0 2) 2 -1
                             (make-vec2 10 1) 1 -1)
      "(0,0) (10,0)")
  
  (ex (circle-circle-tangent (make-vec2 0 1) 1 -1
                             (make-vec2 10 -2) 2 1)
      "(0,0) (10,0)")

  (ex (circle-circle-tangent (make-vec2 0 -1) 1 1
                             (make-vec2 10 2) 2 -1)
      "(0,0) (10,0)")
  
  (ex (circle-circle-tangent (make-vec2 0 1) 1 1
                             (make-vec2 10 1) 1 1)
      "(0,0) (10,0)")
  
  (ex (circle-circle-tangent (make-vec2 10 0) 1 -1
                             (make-vec2 5 1) 1 -1)
      "not crash")
  )