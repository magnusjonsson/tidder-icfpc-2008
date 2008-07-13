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

(define safety-margin 2.0) ; 100% of vehicle's width

(define (safe-radius o)
  (+ safety-margin
     (obj-radius o)))

(define (compute-target pos)
  ; pos = position of our rover
  (safe-point pos (make-vec2 0 0) #f))

; Returns a point that we can safely travel to. For the full pathfinding,
; this needs to generate all suitable points.
(define (safe-point pos target ignore)
  (let ((real-target target)
        (visited (make-hash))
        (obstacle #f)
        (direction 0))
    (when ignore (hash-set! visited ignore #t))
    (let avoidance-loop ()
      (printf ".")
      (let* ((ray (vec2- target pos))
             (b (first-hit-obj pos ray)))
        (when (and b (not (hash-ref visited b #f)))
          (hash-set! visited b #t)
          (let ((t (ray-circle-intersection-first-time
                    pos ray (obj-pos b) (obj-radius b))))
            (when (< t 1)
              ; adjust target to be left tangent point
              ; this is the point where we will need to branch for the full
              ; search, and also consider the right tangent point.
              (set! target (tangent pos (obj-pos b) (safe-radius b) 1))
              (set! obstacle b)
              (set! direction 1)
              (avoidance-loop))))))
    (tangent-point pos real-target obstacle direction)))

; Finds a tangent point on the obstacle with the given direction, keeping an
; eye on the final target. If another obstacle is connected to this one and
; blocks the arc that we want to drive, considers finding a tangent point on
; that obstacle instead.
(define (tangent-point pos target obstacle direction)
  (if (not obstacle) target
      (let* ((p (tangent pos (obj-pos obstacle) (safe-radius obstacle) direction))
             (q (tangent target (obj-pos obstacle) (safe-radius obstacle) (- direction)))
             (arc (curve-angle p (obj-pos obstacle) q (- direction)))
             (arc-max (first-curve-hit-angle p (obj-pos obstacle) (- direction))))
        (printf "obstacle ~a~n" obstacle)
        (cond
          ((or (not arc-max) (<= arc arc-max)) p) ; "easy case"
          (else
           ; the curve around obstacle that we plan to drive is partially blocked
           ; so change your target to aim at the blocker instead
           (printf "blocker ~a~n" (first-curve-hit-obj p (obj-pos obstacle) (- direction)))
           (let* ((blocker (first-curve-hit-obj p (obj-pos obstacle) (- direction)))
                  (t (circle-circle-tangent (obj-pos obstacle) (safe-radius obstacle) direction
                                            (obj-pos blocker) (safe-radius blocker) direction))
                  (target-on-blocker (cdr t)))
             (safe-point pos target-on-blocker blocker)))))))

(define-struct arc (host-obj ccw-obj cw-obj) #:transparent)

(define-struct directed-arc (arc direction) #:transparent)


(define (tangent-info->directed-arc tangent)
  (match tangent
    ((list obj dir tangent-point)
     (let ((arc (make-arc obj 
                          ; todo: figure out what delelimits the arc
                          #f #f)))
       (make-directed-arc arc dir)))))

(define (reachable-states state)
  (match state
    ((struct vec2 (0 0)) ; home
     (list)) ; don't go anywhere after coming home
    ((struct vec2 (_ _)) ; our rover
     (if (not (line-obstructed? state vec2-origin))
         ; if we can go home directly, don't even consider doing something else
         (list vec2-origin)
         ; otherwise find reachable directed arcs
         (let ((reachable-tangents (unobstructed-point-obj-tangents state)))
           (map tangent-info->directed-arc reachable-tangents))))
    ((struct directed-arc (arc direction))
     ; todo: implement
     '())))