#lang scheme

; vec2 models mathematical 2d vectors, i.e. they represent either a point or a
; velocity in the plane.

(provide make-vec2 vec2? vec2-x vec2-y
         vec2- vec2+
         vec2-length vec2-length-squared
         vec2-dot-product
         vec2-scale
         vec2-normalize
         vec2-rotate-ccw-90
         vec2-rotate-cw-90
         vec2-angle-rad
         vec2-angle-deg
         vec2-distance vec2-distance-squared
         angle->vec2
         angle-deg->vec2
         curve-angle
         )

(require "angles.scm")

(define-struct vec2 (x y) #:transparent)

(define (binary-vec2+ a b)
  (make-vec2 (+ (vec2-x a) (vec2-x b)) (+ (vec2-y a) (vec2-y b))))

; note: arguments reversed so that the fold in vec2- works
(define (binary-vec2-sub b a)
  (make-vec2 (- (vec2-x a) (vec2-x b)) (- (vec2-y a) (vec2-y b))))


(define vec2+ (case-lambda
                [() (make-vec2 0 0)]
                [(a) a]
                [(a b) (binary-vec2+ a b)]
                [(a . rest) (foldl binary-vec2+ a rest)]))

(define vec2- (case-lambda
                [(a) (make-vec2 (- (vec2-x a)) (- (vec2-y a)))]
                [(a . rest) (foldl binary-vec2-sub a rest)]))

(define (vec2-scale factor v)
  (make-vec2 (* factor (vec2-x v)) (* factor (vec2-y v))))

(define (vec2-length-squared v)
  (+ (sqr (vec2-x v)) (sqr (vec2-y v))))

(define (vec2-length v)
  (sqrt (vec2-length-squared v)))

(define (vec2-dot-product a b)
  (+ (* (vec2-x a) (vec2-x b)) (* (vec2-y a) (vec2-y b))))

(define (vec2-rotate-ccw-90 a)
  (make-vec2 (- (vec2-y a)) (vec2-x a)))

(define (vec2-rotate-cw-90 a)
  (make-vec2 (vec2-y a) (- (vec2-x a))))

(define (vec2-normalize a)
  (vec2-scale (/ (vec2-length a)) a))

(define (vec2-angle-rad a)
  (atan (vec2-y a) (vec2-x a)))

(define (vec2-angle-deg a)
  (atan-deg (vec2-y a) (vec2-x a)))

(define (vec2-distance-squared a b)
  (+ (sqr (- (vec2-x a) (vec2-x b)))
     (sqr (- (vec2-y a) (vec2-y b)))))

(define (vec2-distance a b)
  (sqrt (vec2-distance-squared a b)))

(define (angle->vec2 rad)
  (make-vec2 (cos rad) (sin rad)))

(define (angle-deg->vec2 deg)
  (angle->vec2 (deg->rad deg)))

(define (deg-normalize-positive deg)
  (- deg (* 360 (floor (/ deg 360)))))

(define (curve-angle curve-start curve-center curve-end direction)
  (let* ((angle1 (vec2-angle-deg (vec2- curve-start curve-center)))
         (angle2 (vec2-angle-deg (vec2- curve-end curve-center))))
    (deg-normalize-positive (* direction (- angle2 angle1)))))
