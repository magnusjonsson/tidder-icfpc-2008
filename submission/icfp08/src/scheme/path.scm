#lang scheme

(require "libraries/search/astar.ss")
(require "vec2.scm")
(require "remember.scm")
(require "intersect.scm")
(require "messages.scm")
(require "tangent.scm")

(provide safety-margin compute-target)

; The idea is to store a "planned path", i.e. the path we will follow if no
; information arrives, and to recompute it whenever the planned path turns out
; to be blocked.

; Disregarding acceleration, an optimal path around an obstacle course is
; always a combination of straight lines tangential to an obstacle, and
; arcs around an obstacle. I think we can make the simplifying assumption
; that this is true even with acceleration.

; Given that, paths are compactly stored as a list of relevant obstacles that
; are circled around, plus the direction in which we avoid the obstacles.
; The empty list corresponds to the path that goes directly to the end point.

(define safety-margin 1.0) ; 100% of vehicle's width

(define (safe-radius o)
  (+ safety-margin
     (obj-radius o)))

(define (compute-target pos)
  ; pos = position of our rover
  (let ((target (make-vec2 0 0))
        (visited (make-hash)))
    (let avoidance-loop ()
      (printf ".")
      (let* ((ray (vec2- target pos))
             (b (first-hit-obj pos ray)))
        (when (and b (not (hash-ref visited b #f)))
          (hash-set! visited b #t)
          (let ((t (ray-circle-intersection-first-time
                    pos ray
                    (obj-pos b) (obj-radius b))))
            (when (< t 1)
              ; adjust target to be the left tangent point
              ; of b
              (set! target (tangent pos (obj-pos b) (safe-radius b) 1))
              (avoidance-loop))))))
    target))
