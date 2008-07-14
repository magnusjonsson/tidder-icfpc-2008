#lang scheme

(require "messages.scm")
(require "vec2.scm")
(require "intersect.scm")
(require "tangent.scm")
(require "misc-syntax.ss")
(require (only-in rnrs/base-6 assert))
(require (prefix-in gfx- "graphics.scm"))

(provide remember-objects remember-object
         print-remembered clear-remembered
         first-hit-obj first-hit-time
         first-curve-hit-angle first-curve-hit-obj
         line-obstructed?
         unobstructed-point-obj-tangents
         unobstructed-obj-obj-tangents
         remembered-dirty?
         clear-remembered-dirty
         draw-remembered
         )

; We store all objects seen.
; The value corresponding to each object is a parent object as in the Union-Find algorithm.
;

(define-struct versioned-tangents (version tangents) #:mutable)

(define-struct info (parent ; used for union-find to group connected objects
                     ; add more here as needed
                     ccw ; versioned-tangents
                     cw ; versioned-tangents
                     neighbours
                     ) #:mutable)

(define obj-count 0)
(define group-count 0)
(define remembered (make-hash))  ; obj -> info

; new objects are pushed here in the order they arrive
(define history '())

(define (objects-added-since version)
  (let ((h history)
        (r '()))
    (while (not (eq? h version))
           (push! r (pop! h)))
    r))

(define (clear-remembered)
  (set! remembered (make-hash))
  (set! history '())
  (set! obj-count 0)
  (set! group-count 0)
  (set! dirty #t))

(define dirty #t)

(define (remembered-dirty?)
  dirty)

(define (clear-remembered-dirty)
  (set! dirty #f))

(define (remember-object o)
  (remember-objects (list o)))

; more efficient than remember-object
(define (remember-objects objects)
  (let ((new-objects (filter (lambda (o)
                               (and (obj? o)
                                    (not (equal? 'home-base (obj-kind o)))
                                    (not (hash-ref remembered o #f))))
                             objects)))
    ;(printf "received new objects: ~a~n" new-objects)
    ;(printf "checking old tangents~n")
    (for-each (lambda (o)
                (assert (obj? o))
                (let ()
                  (hash-set! remembered o
                             (make-info o ; parent
                                        (make-versioned-tangents '() '())
                                        (make-versioned-tangents '() '())
                                        '() ; neighbours
                                        ))
                  ; pushing to history
                  (push! history o)
                  (inc! obj-count)
                  (inc! group-count)
                  ;(printf "merging obj to groups~n")
                  (merge-new-obj o)
                  ;(printf "done merging~n")
                  (set! dirty #t)))
              new-objects)))

(define (test-remember-objects)
  (clear-remembered)
  (remember-objects
   (list (make-obj 'crater (make-vec2 50.0 -5.0) 5.0) (make-obj 'crater (make-vec2 50.0 5.0) 5.0))))

(define (add-neighbor a b)
  (define (add-single-neighbor a b)
    (let ((info (hash-ref remembered a)))
      (set-info-neighbours! info (cons b (info-neighbours info)))))
  (add-single-neighbor a b)
  (add-single-neighbor b a))

(define (merge-new-obj o)
  (hash-for-each remembered
                 (lambda (obj parent)
                   (when (and (not (eq? o obj)) (objects-overlap? o obj))
                     (merge o obj)
                     (add-neighbor o obj)))))


; the Union part of Union-Find
(define (merge a b)
  (let ((a (find a))
        (b (find b)))
    (when (not (equal? a b))
      (when (< (random) 0.5)
        (swap! a b))
      (set-info-parent! (hash-ref remembered a) b)
      (dec! group-count)
      ;(printf "~a objs in ~a groups~n" obj-count group-count)
      (assert (> obj-count group-count 0))
      )))

; the Find part of Union-Find
(define (find o)
  (let* ((o-info (hash-ref remembered o #f))
         (p (info-parent o-info)))
    (if (equal? p o)
        o
        (let ((g (find p)))
          (set-info-parent! o-info g)
          g))))

(define (same-group? a b)
  (eq? (find a) (find b)))

(define (print-remembered)
  (printf "remembered objects:")
  (hash-for-each remembered
                 (lambda (key value)
                   (printf " ~a" key)))
  (printf "~n"))

(define (line-obstructed? p0 p1 (tabu-obj-list '()))
  (let ((t (first-hit-time p0 (vec2- p1 p0) tabu-obj-list)))
    (and t (< t 1))))

(define (first-hit-time origin ray (tabu-obj-list '()))
  (let ((b (first-hit-obj origin ray tabu-obj-list)))
    (and b (ray-circle-intersection-first-time origin ray (obj-pos b) (obj-radius b)))))

(define (first-hit-obj origin ray (tabu-obj-list '()))
  (let ((best-time #f)
        (best-obj #f))
    (hash-for-each remembered
                   (lambda (obj parent)
                     ; if there's an intersection that happens before
                     ; target-distance, (return obj)
                     (when (not (member obj tabu-obj-list))
                       (let ((t (ray-circle-intersection-first-time
                                 origin ray (obj-pos obj) (obj-radius obj))))
                         (when t
                           (unless (and best-time (< best-time t))
                             (set! best-obj obj)
                             (set! best-time t)))))))
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

(define (first-curve-hit-obj obj curve-start curve-center direction)
  (let ((best-angle #f)
        (best-obj #f))
    ; only check neighbors
    (for-each (lambda (o)
                (let ((t (curve-circle-intersection-angle2
                          curve-start curve-center (obj-radius obj) direction
                          (obj-pos o) (obj-radius o))))
                  (when t
                    (unless (and best-angle (< best-angle t))
                      (set! best-angle t)
                      (set! best-obj o)))))
              (info-neighbours (hash-ref remembered obj)))
    best-obj))

(define (objects-overlap? o1 o2)
  (<= (vec2-distance (obj-pos o1) (obj-pos o2))
      (+ (obj-radius o1) (obj-radius o2))))

(define (unobstructed-point-obj-tangents point)
  (let ((result '()))
    (define (consider obj dir)
      (let ((tangent-point (tangent point (obj-pos obj) (obj-radius obj) (- dir))))
        (unless (line-obstructed? point tangent-point (list obj))
          (push! result (list obj dir tangent-point)))))
    (hash-for-each remembered
                   (lambda (obj _)
                     (consider obj -1)
                     (consider obj 1)))
    result))
  
(define (unobstructed-obj-obj-tangents obj1 dir1)
  (let* ((info (hash-ref remembered obj1))
         (versioned-tangents (match dir1
                              (1 (info-ccw info))
                              (-1 (info-cw info)))))
    (bring-tangents-up-to-date versioned-tangents obj1 dir1)
    (versioned-tangents-tangents versioned-tangents)))

(define (bring-tangents-up-to-date vt obj1 dir1)
  (let ((new-objs (objects-added-since (versioned-tangents-version vt))))
    (set-versioned-tangents-tangents!
     vt
     (append (discover-new-tangents obj1 dir1 new-objs)
             (filter-old-tangents (versioned-tangents-tangents vt) new-objs)))
    (set-versioned-tangents-version! vt history)))

(define (discover-new-tangents obj1 dir1 new-objs)
  (let ((result '()))
    (define (consider obj2 dir2)
      (let ((tangent-points (circle-circle-tangent (obj-pos obj1) (obj-radius obj1) dir1
                                                   (obj-pos obj2) (obj-radius obj2) dir2)))
        (unless (line-obstructed? (car tangent-points) (cdr tangent-points) (list obj1 obj2))
          (push! result (list (car tangent-points)
                              obj2 dir2 (cdr tangent-points))))))
    (for-each (lambda (obj2)
                (when (not (equal? obj1 obj2))
                  ; straight loop
                  (consider obj2 dir1)
                  (unless (objects-overlap? obj1 obj2)
                    ; zig-zagging between overlapping objects doesn't
                    ; make sense
                    (consider obj2  (- dir1)))))
              new-objs)
    result))

(define (filter-old-tangents tangents new-objs)
  (define (clear? tangent)
    (match tangent
      ((list point1 obj2 dir2 point2)
       (not (ormap (lambda (o)
                     (line-intersects-circle? point1 point2 (obj-pos o) (obj-radius o)))
                   new-objs)))))
  (filter clear? tangents))

(define (test)
  (test1)
  (test2)
  (test3)
  (test-remember-objects))

(define (test1)
  (clear-remembered)
  (remember-objects (list (make-obj 'crater (make-vec2 10 0) 1)))
  (printf "~a~n~n" (unobstructed-point-obj-tangents (make-vec2 0 0)))
  (remember-objects (list (make-obj 'crater (make-vec2 5 1) 1)))
  (printf "~a~n~n" (unobstructed-point-obj-tangents (make-vec2 0 0))))

(define (test2)
  (clear-remembered)
  (let ((o1 (make-obj 'crater (make-vec2 10 0) 1))
        (o2 (make-obj 'crater (make-vec2 5 0) 2))
        (o3 (make-obj 'crater (make-vec2 0 0) 1)))
    (remember-objects (list o1 o2 o3))
    (assert (line-obstructed? (make-vec2 2 0) (make-vec2 8 0) (list)))
    (assert (line-obstructed? (make-vec2 2 0) (make-vec2 8 0) (list o1 o3)))
    (assert (not (line-obstructed? (make-vec2 2 0) (make-vec2 8 0) (list o1 o2 o3))))
    ))

(define (test3)
  (define (pretty-list-of-length correct-length list)
    (printf "list:~n")
    (dolist (i list)
            (printf "--> ~a~n" i))
    (assert (= (length list) correct-length)))
  (clear-remembered)
  (let ((o1 (make-obj 'crater (make-vec2 10 0) 1))
        (o2 (make-obj 'crater (make-vec2 0 0) 1)))
    (remember-objects (list o1 o2))
    (pretty-list-of-length 2 (unobstructed-obj-obj-tangents o1 -1))
    (pretty-list-of-length 2 (unobstructed-obj-obj-tangents o1 1))
    (pretty-list-of-length 2 (unobstructed-obj-obj-tangents o2 -1))
    (pretty-list-of-length 2 (unobstructed-obj-obj-tangents o2 1))
    
    ; put something between them
    (let ((o3 (make-obj 'crater (make-vec2 5 0) 2)))
      (remember-object o3)
      (pretty-list-of-length 2 (unobstructed-obj-obj-tangents o1 -1))
      (pretty-list-of-length 2 (unobstructed-obj-obj-tangents o1 1))
      (pretty-list-of-length 2 (unobstructed-obj-obj-tangents o2 -1))
      (pretty-list-of-length 2 (unobstructed-obj-obj-tangents o2 1))
      (pretty-list-of-length 4 (unobstructed-obj-obj-tangents o3 -1))
      (pretty-list-of-length 4 (unobstructed-obj-obj-tangents o3 1))
      )))



(define (draw-remembered)
  (when (gfx-on?)
    (hash-for-each remembered
                   (lambda (obj _)
                     (let ((p (obj-pos obj)))
                       (gfx-circle (vec2-x p) (vec2-y p) (obj-radius obj)))))))
