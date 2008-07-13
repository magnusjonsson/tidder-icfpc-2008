#lang scheme

(require "messages.scm")
(require "vec2.scm")

(provide remembered remember-objects remember-object
         print-remembered clear-remembered)

(define remembered (make-hash))

(define (remember-objects objects)
  (for-each remember-object objects))

(define (remember-object o)
  (when (and (obj? o) (not (hash-ref remembered o #f)))
    (hash-set! remembered o #t)
    ;update path here
    ))

(define (print-remembered)
  (printf "remembered objects:")
  (hash-for-each remembered
                 (lambda (key value)
                   (printf " ~a" key)))
  (printf "~n"))

(define (clear-remembered)
  (set! remembered (make-hash)))


(define (objects-overlap? o1 o2)
  (<= (vec2-distance (obj-pos o1) (obj-pos o2))
      (+ (obj-radius o1) (obj-radius o2))))
