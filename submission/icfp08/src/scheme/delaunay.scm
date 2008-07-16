#lang scheme

; http://en.wikipedia.org/wiki/Delaunay_triangulation
;
; This algo works by adding a vertex to the middle of a
; triangle and then fixing resulting triangles which are not good.

(require "misc-syntax.ss")
(require "vec2.scm")
(require (prefix-in gfx- "graphics.scm"))

(define-struct triangle (v1 v2 v3) #:transparent)


(define ccw-vertex-hash (make-hash)) ; (cons v1 v2) -> v3  where v1 v2 v3 form a ccw triangle

(define (ccw-vertex-for-each thunk)
  (hash-for-each ccw-vertex-hash thunk))

(define (set-ccw-vertex! v1 v2 v3)
  (hash-set! ccw-vertex-hash (cons v1 v2) v3))
(define (ccw-vertex v1 v2)
  (hash-ref ccw-vertex-hash (cons v1 v2) #f))

(define (remove-ccw-vertex! v1 v2)
  (hash-remove! ccw-vertex-hash (cons v1 v2)))

(define (set-triangle! v1 v2 v3)
  (set-ccw-vertex! v1 v2 v3)
  (set-ccw-vertex! v2 v3 v1)
  (set-ccw-vertex! v3 v1 v2))

(define (remove-triangle! v1 v2 v3)
  (remove-ccw-vertex! v1 v2)
  (remove-ccw-vertex! v2 v3)
  (remove-ccw-vertex! v3 v1))

(define (init!)
  (set! ccw-vertex-hash (make-hash))
  (let ((v1 (make-vec2 -1  1))  (v2 (make-vec2  1  1))
        (v3 (make-vec2 -1 -1))  (v4 (make-vec2  1 -1)))
    (set-triangle! v1 v3 v2)
    (set-triangle! v3 v4 v2)
    ))
(init!)

(define (some-random-edge)
  (let/ec found
    (ccw-vertex-for-each (lambda (edge v3)
                           (found edge)))
    #f))

(define (add-vertex! v)
  (match (vertex->triangle v)
    ((list v1 v2 v3)
     (remove-triangle! v1 v2 v3)
     (set-triangle! v1 v2 v)
     (set-triangle! v2 v3 v)
     (set-triangle! v3 v1 v)
     (fix-edges! (list (cons v1 v2) (cons v2 v3) (cons v3 v1))))))

(define (vertex->triangle v)
  (match (some-random-edge)
    ((cons v1 v2)
     (if (ccw? v1 v2 v)
         (chase v1 v2 v)
         (chase v2 v1 v)))))

(define (chase v1 v2 v)
  ; find the triangle that contains v.
  ; it is known that v1 v2 is an edge and v1->v2->v3 is ccw
  (let ((v3 (ccw-vertex v1 v2)))
    (and v3
         (cond
           ((not (ccw? v2 v3 v)) (chase v3 v2 v))
           ((not (ccw? v3 v1 v)) (chase v1 v3 v))
           (else (list v1 v2 v3))))))

(define (inside-triangle? t v)
  (let ((v1 (triangle-v1 t))
        (v2 (triangle-v2 t))
        (v3 (triangle-v3 t)))
    (and (ccw? v1 v2 v)
         (ccw? v2 v3 v)
         (ccw? v3 v1 v))))

(define (ccw? v1 v2 v3)
  (>= (vec2-dot-product (vec2-rotate-ccw-90 (vec2- v2 v1))
                        (vec2- v3 v1))
      0))

(define (test-ccw?)
  (ccw? (make-vec2 0 0) (make-vec2 1 0) (make-vec2 0 1)))


(define (edge-broken? v1 v2)
  ; an edge is broken if the sum of the angles of its its ccw and cw vertices
  ; is greater than 180
  (define (vec2->complex v)
    (make-rectangular (vec2-x v) (vec2-y v)))
  (let ((v3 (ccw-vertex v1 v2))
        (v4 (ccw-vertex v2 v1)))
    (and v3
         v4
         (negative? (imag-part (* (vec2->complex (vec2- v2 v3))
                                  (conjugate (vec2->complex (vec2- v1 v3)))
                                  (vec2->complex (vec2- v1 v4))
                                  (conjugate (vec2->complex (vec2- v2 v4)))))))))

(define (fix-edges! edges)
  (define (flip-edge! v1 v2)
    (let ((v3 (ccw-vertex v1 v2))
          (v4 (ccw-vertex v2 v1)))
      (remove-triangle! v1 v2 v3)
      (remove-triangle! v2 v1 v4)
      (set-triangle! v1 v4 v3)
      (set-triangle! v2 v3 v4)
      (push! edges (cons v1 v3))
      (push! edges (cons v3 v2))
      (push! edges (cons v2 v4))
      (push! edges (cons v4 v1))))
  (while (cons? edges)
         (match (pop! edges)
           ((cons v1 v2)
            (when (edge-broken? v1 v2)
              (flip-edge! v1 v2))))))
      
    
(define (draw!)
  (when (gfx-on?)
    (gfx-color 255 255 255)
    (gfx-clear)
    (gfx-color 0 0 0)
    (let ((visited (make-hash)))
      (define todo (list (some-random-edge)))
      (while (cons? todo)
             (let ((e (pop! todo)))
               (unless (hash-ref visited e #f)
                 (hash-set! visited e #t)
                 (let ((v1 (car e))
                       (v2 (cdr e)))
                   (gfx-line (vec2-x v1) (vec2-y v1) (vec2-x v2) (vec2-y v2))
                   (let ((v3 (ccw-vertex v1 v2)))
                     (when v3
                       (push! todo (cons v1 v3))
                       (push! todo (cons v2 v3))))
                   (let ((v3 (ccw-vertex v2 v1)))
                     (when v3
                       (push! todo (cons v1 v3))
                       (push! todo (cons v2 v3)))))))))
    (gfx-show)))

(define (test)
  (init!)
  (gfx-init "../../../../stuff/graphics/g")
  (gfx-dx 2)
  (gfx-dy 2)
  (dotimes (i 1000)
           (let ((v (make-vec2 (+ -1 (* 2 (random)))
                               (+ -1 (* 2 (random))))))
;             (printf "~a~n" v)
             (add-vertex! v))
           (when (= 99 (modulo i 100))
             (draw!))))