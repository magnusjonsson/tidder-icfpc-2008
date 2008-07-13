#lang scheme

(require "messages.scm")
(require "vec2.scm")
(require "intersect.scm")

(provide remember-objects remember-object
         print-remembered clear-remembered
         first-hit-obj first-hit-time)

(define remembered (make-hash))

(define (remember-objects objects)
  (for-each remember-object objects))

(define (remember-object o)
  (when (and (obj? o)
             (not (equal? 'home-base (obj-kind o)))
             (not (hash-ref remembered o #f)))
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

(define (first-hit-time origin ray)
  (let ((b (first-hit-obj origin ray)))
    (and b (ray-circle-intersection-first-time origin ray (obj-pos b) (obj-radius b)))))

(define (first-hit-obj origin ray)
  (let ((best-time #f)
        (best-obj #f))
    (hash-for-each remembered
                   (lambda (obj junk)
                     ; if there's an intersection that happens before
                     ; target-distance, (return obj)
                     (let ((t (ray-circle-intersection-first-time
                               origin ray (obj-pos obj) (obj-radius obj))))
                       (when t
                         (unless (and best-time (< best-time t))
                           (set! best-obj obj)
                           (set! best-time t))))))
    best-obj))
    

(define (objects-overlap? o1 o2)
  (<= (vec2-distance (obj-pos o1) (obj-pos o2))
      (+ (obj-radius o1) (obj-radius o2))))
