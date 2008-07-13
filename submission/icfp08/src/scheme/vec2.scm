#lang scheme

; vec2 models mathematical 2d vectors, i.e. they represent either a point or a
; velocity in the plane.

(provide make-vec2 vec2? vec2-x vec2-y
         vec2- vec2+
         vec2-length vec2-length-squared
         vec2-rotate-ccw-90
         vec2-rotate-cw-90)

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
  (make-vec2 (* (vec2-x a) (vec2-x b)) (* (vec2-y a) (vec2-y b))))

(define (vec2-rotate-ccw-90 a)
  (make-vec2 (- (vec2-y a)) (vec2-x a)))

(define (vec2-rotate-cw-90 a)
  (make-vec2 (vec2-y a) (- (vec2-x a))))

(define (vec2-normalize a)
  (vec2-scale (/ (vec2-length a)) a))
  