#lang scheme

(require "messages.scm")
(require "vec2.scm")
(require "intersect.scm")
(require "misc-syntax.ss")

(provide remember-objects remember-object
         print-remembered clear-remembered
         first-hit-obj first-hit-time
         first-curve-hit-angle first-curve-hit-obj)

; We store all objects seen.
; The value corresponding to each object is a parent object as in the Union-Find algorithm.
;
; The Union-Find algorithm is used to merge groups.
(define remembered (make-hash))

(define (remember-objects objects)
  (for-each remember-object objects))

(define (remember-object o)
  (when (and (obj? o)
             (not (equal? 'home-base (obj-kind o)))
             (not (hash-ref remembered o #f)))
    (hash-set! remembered o o)
    ; possibly merge it with existing groups
    (merge-new-obj o)
    ;update path here
    ))

(define (merge-new-obj o)
  (hash-for-each remembered
                 (lambda (obj parent)
                   (when (objects-overlap? o obj)
                     (merge o obj)))))


; the Union part of Union-Find
(define (merge a b)
  (let ((a (find a))
        (b (find b)))
    (when (< (random) 0.5)
      (swap! a b))
    (hash-set! remembered a b)))

; the Find part of Union-Find
(define (find o)
  (let ((p (hash-ref remembered o #f)))
    (if (equal? p o)
        o
        (let ((g (find p)))
          (hash-set! remembered o g)
          g))))

(define (same-group? a b)
  (eq? (find a) (find b)))

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
                   (lambda (obj parent)
                     ; if there's an intersection that happens before
                     ; target-distance, (return obj)
                     (let ((t (ray-circle-intersection-first-time
                               origin ray (obj-pos obj) (obj-radius obj))))
                       (when t
                         (unless (and best-time (< best-time t))
                           (set! best-obj obj)
                           (set! best-time t))))))
    best-obj))

(define (first-curve-hit-angle curve-start curve-center direction)
  (let ((best-angle #f))
    ; possible optimization: only check adjacent obstacles
    (hash-for-each remembered
                   (lambda (obj _)
                     (let ((t (curve-circle-intersection-angle
                               curve-start curve-center direction
                               (obj-pos obj) (obj-radius obj))))
                       (when t
                         (unless (and best-angle (< best-angle t))
                           (set! best-angle t))))))
    best-angle))

(define (first-curve-hit-obj curve-start curve-center direction)
  (let ((best-angle #f)
        (best-obj #f))
    ; possible optimization: only check adjacent obstacles
    (hash-for-each remembered
                   (lambda (obj _)
                     (let ((t (curve-circle-intersection-angle
                               curve-start curve-center direction
                               (obj-pos obj) (obj-radius obj))))
                       (when t
                         (unless (and best-angle (< best-angle t))
                           (set! best-angle t)
                           (set! best-obj obj))))))
    best-obj))

(define (objects-overlap? o1 o2)
  (<= (vec2-distance (obj-pos o1) (obj-pos o2))
      (+ (obj-radius o1) (obj-radius o2))))
