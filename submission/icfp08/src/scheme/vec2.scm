#lang scheme

; vec2 models mathematical 2d vectors, i.e. they represent either a point or a
; velocity in the plane.

(provide make-vec2 vec2? vec2-x vec2-y vec2- vec2+)

(define-struct vec2 (x y) #:transparent)

(define vec2+ (case-lambda
                [() (make-vec2 0 0)]
                [(a) a]
                [(a b) (make-vec2 (+ (vec2-x a) (vec2-x b)) (+ (vec2-y a) (vec2-y b)))]
                [(a b . rest) (apply vec2+ (vec2+ a b) rest)]))

(define vec2- (case-lambda
                [(a) (make-vec2 (- (vec2-x a)) (- (vec2-y a)))]
                [(a . rest) (vec2+ a (vec2- (apply vec2+ rest)))]))
